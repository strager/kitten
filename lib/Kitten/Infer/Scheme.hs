{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE PatternGuards #-}
{-# LANGUAGE PostfixOperators #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Kitten.Infer.Scheme
  ( Instantiation(..)
  , Occurrences(..)
  , Simplify(..)
  , Substitute(..)
  , free
  , generalize
  , instantiate
  , instantiateM
  , instantiatedM
  , normalize
  , occurs
  ) where

import Control.Applicative ((<$>), (<*>))
import Control.Monad.Trans.State.Strict
import Data.List
import Data.Monoid
import Data.Set (Set)

import qualified Data.Set as S

import Kitten.Infer.Monad
import Kitten.Name
import Kitten.Type
import Kitten.Type.Tidy
import Kitten.Typed (VarInstantiations(..), VarKindInstantiations(..))
import Kitten.Util.Monad

import qualified Kitten.NameMap as N

data Instantiation = Instantiation
  { instantiatedType :: Type Scalar
  , tidiedType :: Type Scalar
  , varInstantiations :: VarInstantiations
  -- ^ Mapping from 'tidiedType' to 'instantiatedType'.
  }

instantiateM
  :: TypeScheme
  -> Inferred Instantiation
instantiateM scheme = do
  origin <- getsEnv envOrigin
  liftState $ state (instantiate origin scheme)

instantiatedM :: TypeScheme -> Inferred (Type Scalar)
instantiatedM = fmap instantiatedType . instantiateM

instantiate :: Origin -> TypeScheme -> Env -> (Instantiation, Env)
instantiate origin scheme env = (instantiation, env')
  where
  instantiation :: Instantiation
  instantiation = Instantiation
    { instantiatedType = sub (varInstantiationsEnv vars) tidied
    , tidiedType = tidied
    , varInstantiations = vars
    }

  vars :: VarInstantiations
  env' :: Env
  (vars, env') = flip runState env
    $ VarInstantiations
      <$> renames rows
      <*> renames scalars

  renames
    :: Set (TypeName a)
    -> State Env (VarKindInstantiations a)
  renames = foldMapM rename . S.toList

  rename
    :: TypeName a
    -> State Env (VarKindInstantiations a)
  rename name = do
    var <- state (freshVar origin)
    return . VarKindInstantiations
      $ N.singleton (unTypeName name) var

  Forall rows scalars tidied = runTidy
    $ tidyScheme tidyScalarType scheme

-- | Creates an inference environment with the given
-- unifications applied.
varInstantiationsEnv :: VarInstantiations -> Env
varInstantiationsEnv (VarInstantiations rows scalars)
  = emptyEnv
  { envRows = instantiationNameMap rows
  , envScalars = instantiationNameMap scalars
  }

generalize :: Type Scalar -> Inferred TypeScheme
generalize type_ = do
  after <- getEnv

  let
    substituted :: Type Scalar
    substituted = sub after type_

    rows :: [TypeName Row]
    scalars :: [TypeName Scalar]
    (rows, scalars) = freeVars substituted

  return $ Forall
    (S.fromList rows)
    (S.fromList scalars)
    substituted

freeVars
  :: (Free a)
  => Type a
  -> ([TypeName Row], [TypeName Scalar])
freeVars type_
  = let (rows, scalars) = free type_
  in (nub rows, nub scalars)

class Free a where
  free
    :: Type a
    -> ([TypeName Row], [TypeName Scalar])

instance Free Row where
  free type_ = case type_ of
    a :. b -> free a <> free b
    Empty{} -> mempty
    Var name _ -> ([name], [])

instance Free Scalar where
  free type_ = case type_ of
    a :& b -> free a <> free b
    (:?) a -> free a
    a :| b -> free a <> free b
    Function a b _ -> free a <> free b
    Bool{} -> mempty
    Char{} -> mempty
    Float{} -> mempty
    Handle{} -> mempty
    Int{} -> mempty
    Named{} -> mempty
    Var name _ -> ([], [name])
    Unit{} -> mempty
    Vector a _ -> free a

normalize :: Origin -> Type Scalar -> Type Scalar
normalize origin type_ = let
  (rows, scalars) = freeVars type_
  rowCount = length rows
  env = emptyEnv
    { envRows = N.fromList
      $ zip (map unTypeName rows) (map var [0..])
    , envScalars = N.fromList
      $ zip (map unTypeName scalars) (map var [rowCount..])
    }
  in sub env type_
  where
  var :: Int -> Type a
  var index = Var (TypeName (Name index)) origin

occurs :: (Occurrences a) => Name -> Env -> Type a -> Bool
occurs = (((> 0) .) .) . occurrences

class Occurrences a where
  occurrences :: Name -> Env -> Type a -> Int

instance Occurrences Row where
  occurrences name env type_ = case type_ of
    a :. b -> occurrences name env a + occurrences name env b
    Empty{} -> 0
    Var typeName@(TypeName name') _ -> case retrieve env typeName of
      Left{} -> if name == name' then 1 else 0  -- See Note [Var Kinds].
      Right type' -> occurrences name env type'

instance Occurrences Scalar where
  occurrences name env type_ = case type_ of
    a :& b -> occurrences name env a + occurrences name env b
    (:?) a -> occurrences name env a
    a :| b -> occurrences name env a + occurrences name env b
    Function a b _ -> occurrences name env a + occurrences name env b
    Bool{} -> 0
    Char{} -> 0
    Float{} -> 0
    Handle{} -> 0
    Int{} -> 0
    Named{} -> 0
    Var typeName@(TypeName name') _ -> case retrieve env typeName of
      Left{} -> if name == name' then 1 else 0  -- See Note [Var Kinds].
      Right type' -> occurrences name env type'
    Unit{} -> 0
    Vector a _ -> occurrences name env a

-- Note [Var Kinds]:
--
-- Type variables are allocated such that, if the 'Kind's of
-- two type variables are not equal, the 'Names' of those
-- two type variables are not equal.

class Simplify a where
  simplify :: Env -> Type a -> Type a

instance Simplify Row where
  simplify env type_ = case type_ of
    Var name (Origin hint _loc)
      | Right type' <- retrieve env name
      -> simplify env type' `addHint` hint
    _ -> type_

instance Simplify Scalar where
  simplify env type_ = case type_ of
    Var name (Origin hint _loc)
      | Right type' <- retrieve env name
      -> simplify env type' `addHint` hint
    _ -> type_

class Substitute a where
  sub :: Env -> Type a -> Type a

instance Substitute Row where
  sub env type_ = case type_ of
    a :. b -> sub env a :. sub env b
    Empty{} -> type_
    Var name (Origin hint _loc)
      | Right type' <- retrieve env name
      -> sub env type' `addHint` hint
      | otherwise
      -> type_

instance Substitute Scalar where
  sub env type_ = case type_ of
    Function a b origin -> Function
      (sub env a)
      (sub env b)
      origin
    a :& b -> sub env a :& sub env b
    (:?) a -> (sub env a :?)
    a :| b -> sub env a :| sub env b
    Bool{} -> type_
    Char{} -> type_
    Float{} -> type_
    Int{} -> type_
    Handle{} -> type_
    Named{} -> type_
    Var name (Origin hint _loc)
      | Right type' <- retrieve env name
      -> sub env type' `addHint` hint
      | otherwise
      -> type_
    Unit{} -> type_
    Vector a origin -> Vector (sub env a) origin
