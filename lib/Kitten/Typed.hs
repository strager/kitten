{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeFamilies #-}

-- NOTE(strager): The use of this extension (instance of
-- ToText VarKindInstantiations) is safe (I believe).
{-# LANGUAGE UndecidableInstances #-}

module Kitten.Typed
  ( Typed(..)
  , TypedDef
  , Value(..)
  , VarInstantiations(..)
  , VarKindInstantiations(..)
  , defTypeScheme
  , rowInstantiations
  , typedType
  ) where

import Control.Applicative ((<$))
import Data.Monoid
import Data.Text (Text)
import Data.Vector (Vector)

import qualified Data.Text as Text

import Kitten.AST
import Kitten.Builtin (Builtin)
import Kitten.ClosedName
import Kitten.Def
import Kitten.Location
import Kitten.Name
import Kitten.NameMap (NameMap)
import Kitten.Type hiding (Annotated(..), Hint(..))
import Kitten.Util.Text (ToText(..), showText)

import qualified Kitten.NameMap as NameMap

data Typed
  = Builtin !Builtin !Location (Type Scalar)
  | Call !Name !Location VarInstantiations (Type Scalar)
  | Compose !(Vector Typed) !Location (Type Scalar)
  | From !Text !Location (Type Scalar)
  | PairTerm !Typed !Typed !Location (Type Scalar)
  | Push !Value !Location (Type Scalar)
  | To !Text !Location (Type Scalar)
  | Scoped !Typed !Location (Type Scalar)
  | VectorTerm !(Vector Typed) !Location (Type Scalar)
  deriving (Eq, Show)

-- TODO(strager)
instance ToText Typed where
  toText = showText

instance AST Typed where
  type TermValue Typed = Value
  type TermDef Typed = TypedDef

data Value
  = Bool !Bool
  | Char !Char
  | Closed !Name
  | Closure !(Vector ClosedName) !Typed
  | Float !Double
  | Int !Int
  | Local !Name
  | Unit
  | String !Text
  deriving (Eq, Show)

type TypedDef = Def (Scheme Typed)

-- | Substitutions of function variables at a specific call
-- site.
--
-- For example, given:
--
--   def map ((a -> b) [a] -> [b])
--
-- and a call:
--
--   {showInt} [1, 2, 3] map
--
-- the instantiations of 'map' are:
--
--   a -> Int
--   b -> String
newtype VarKindInstantiations (a :: Kind)
  = VarKindInstantiations
    { instantiationNameMap :: NameMap (Type a) }
  deriving (Eq, Monoid)

instance (ToText (Type a)) => Show (VarKindInstantiations a) where
  show = Text.unpack . toText

instance (ToText (Type a)) => ToText (VarKindInstantiations a) where
  toText
    = Text.intercalate ", "
    . map (\(from, to) -> toText from <> " -> " <> toText to)
    . NameMap.toList . instantiationNameMap

data VarInstantiations = VarInstantiations
  (VarKindInstantiations Row)
  (VarKindInstantiations Scalar)
  (VarKindInstantiations Effect)
  deriving (Eq)

instance Monoid VarInstantiations where
  mempty = VarInstantiations mempty mempty mempty
  VarInstantiations r s e `mappend` VarInstantiations r' s' e'
    = VarInstantiations (r <> r') (s <> s') (e <> e')

instance Show VarInstantiations where
  show = Text.unpack . toText

instance ToText VarInstantiations where
  toText (VarInstantiations rows scalars effects)
    = Text.intercalate ", "
      [toText rows, toText scalars, toText effects]

defTypeScheme :: TypedDef -> TypeScheme
defTypeScheme def = type_ <$ defTerm def
  where
  type_ = typedType $ unScheme (defTerm def)

rowInstantiations
  :: VarInstantiations -> VarKindInstantiations Row
rowInstantiations (VarInstantiations rows _ _) = rows

typedType :: Typed -> Type Scalar
typedType typed = case typed of
  Builtin _ _ t -> t
  Call _ _ _ t -> t
  Compose _ _ t -> t
  From _ _ t -> t
  PairTerm _ _ _ t -> t
  Push _ _ t -> t
  To _ _ t -> t
  Scoped _ _ t -> t
  VectorTerm _ _ t -> t
