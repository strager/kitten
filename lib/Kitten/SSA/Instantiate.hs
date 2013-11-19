{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE RecordWildCards #-}

module Kitten.SSA.Instantiate
  ( instantiateTemplate
  ) where

import Control.Applicative ((<$>), (<*>), pure)
import Control.Monad.Trans.Reader
import Data.Monoid ((<>))
import Data.Vector (Vector)
import Unsafe.Coerce (unsafeCoerce)

import qualified Data.Map as Map
import qualified Data.Vector as V

import Kitten.SSA.Types

type InstantiateM = Reader (TemplateArguments Template)

class Instantiate (c :: Form -> *) where
  instantiate :: c Template -> InstantiateM (c Normal)

instance Instantiate Function where
  instantiate Function{..} = do
    -- TODO(strager): Assert that funcTemplateParameters
    -- matches the instantiate context.

    info <- instantiate funcInfo
    instructions <- V.mapM instantiate funcInstructions
    let closures = V.empty  -- TODO(strager)
    return Function
      { funcInstructions = instructions
      , funcClosures = closures
      , funcInfo = info
      }

instance Instantiate FunctionInfo where
  instantiate FunctionInfo{..} = do
    inputs <- instantiate funcInputs
    outputs <- instantiate funcOutputs
    return FunctionInfo
      { funcInputs = inputs
      , funcOutputs = outputs
      , funcTemplateParameters = NoParameters
      , funcLocation = funcLocation
      }

instance Instantiate RowArity where
  instantiate = \case
    ScalarArity arity -> return $ ScalarArity arity
    TemplateArity var arity -> do
      RowArg (ScalarArity rowArity) <- parameter var
      return $ ScalarArity (rowArity + arity)

instance Instantiate Instruction where
  instantiate = \case
    Activation name captured out loc
      -> return $ Activation name captured out loc
    Bool value out loc -> return $ Bool value out loc
    Char value out loc -> return $ Char value out loc
    Call functionName inputs outputs loc
      -- TODO(strager): Instantiate functionName.
      -- FIXME(strager): HACK!
      -> Call (unsafeCoerce functionName)
        <$> instantiate inputs
        <*> instantiate outputs
        <*> pure loc
    CallBuiltin builtinCall loc
      -> CallBuiltin
        <$> instantiate builtinCall
        <*> pure loc
    Float value out loc -> return $ Float value out loc
    Int value out loc -> return $ Int value out loc
    PairTerm a b out loc -> return $ PairTerm a b out loc
    Return inputs loc
      -> Return
        <$> instantiate inputs
        <*> pure loc
    Vector values out loc -> return $ Vector values out loc

instance Instantiate RowVar where
  instantiate = \case
    ScalarVars vars -> return $ ScalarVars vars
    TemplateRowScalarVars row vars -> do
      rowVars <- instantiateRowVar row
      return $ ScalarVars (rowVars <> vars)

instance Instantiate BuiltinCall where
  instantiate = \case
    AddFloat a b out     -> return $ AddFloat out a b
    AddInt a b out       -> return $ AddInt out a b
    AddVector a b out    -> return $ AddVector out a b
    AndBool a b out      -> return $ AndBool out a b
    AndInt a b out       -> return $ AndInt out a b
    CharToInt a out      -> return $ CharToInt out a
    Close a              -> return $ Close a
    DivFloat a b out     -> return $ DivFloat out a b
    DivInt a b out       -> return $ DivInt out a b
    EqFloat a b out      -> return $ EqFloat out a b
    EqInt a b out        -> return $ EqInt out a b
    Exit a               -> return $ Exit a
    First a out          -> return $ First out a
    FromLeft a out       -> return $ FromLeft out a
    FromRight a out      -> return $ FromRight out a
    FromSome a out       -> return $ FromSome out a
    GeFloat a b out      -> return $ GeFloat out a b
    GeInt a b out        -> return $ GeInt out a b
    Get a b out          -> return $ Get out a b
    GetLine a out        -> return $ GetLine out a
    GtFloat a b out      -> return $ GtFloat out a b
    GtInt a b out        -> return $ GtInt out a b
    Init a out           -> return $ Init out a
    IntToChar a out      -> return $ IntToChar out a
    LeFloat a b out      -> return $ LeFloat out a b
    LeInt a b out        -> return $ LeInt out a b
    MakeLeft a out       -> return $ MakeLeft out a
    Length a out         -> return $ Length out a
    LtFloat a b out      -> return $ LtFloat out a b
    LtInt a b out        -> return $ LtInt out a b
    ModFloat a b out     -> return $ ModFloat out a b
    ModInt a b out       -> return $ ModInt out a b
    MulFloat a b out     -> return $ MulFloat out a b
    MulInt a b out       -> return $ MulInt out a b
    NeFloat a b out      -> return $ NeFloat out a b
    NeInt a b out        -> return $ NeInt out a b
    NegFloat a out       -> return $ NegFloat out a
    NegInt a out         -> return $ NegInt out a
    None out             -> return $ None out
    NotBool a out        -> return $ NotBool out a
    NotInt a out         -> return $ NotInt out a
    OpenIn out           -> return $ OpenIn out
    OpenOut out          -> return $ OpenOut out
    OrBool a b out       -> return $ OrBool out a b
    OrInt a b out        -> return $ OrInt out a b
    Pair a b out         -> return $ Pair out a b
    Print a b            -> return $ Print a b
    Rest a out           -> return $ Rest out a
    MakeRight a out      -> return $ MakeRight out a
    Set a b c out        -> return $ Set out a b c
    ShowFloat a out      -> return $ ShowFloat out a
    ShowInt a out        -> return $ ShowInt out a
    Some a out           -> return $ Some out a
    Stderr out           -> return $ Stderr out
    Stdin out            -> return $ Stdin out
    Stdout out           -> return $ Stdout out
    SubFloat a b out     -> return $ SubFloat out a b
    SubInt a b out       -> return $ SubInt out a b
    Tail a out           -> return $ Tail out a
    UnsafePurify11 a out -> return $ UnsafePurify11 out a
    XorBool a b out      -> return $ XorBool out a b
    XorInt a b out       -> return $ XorInt out a b

    Apply func inputs outputs
      -> Apply func
        <$> instantiate inputs
        <*> instantiate outputs
    Choice leftFunc cond inputs outputs
      -> Choice leftFunc cond
        <$> instantiate inputs
        <*> instantiate outputs
    ChoiceElse rightFunc leftFunc cond inputs outputs
      -> ChoiceElse rightFunc leftFunc cond
        <$> instantiate inputs
        <*> instantiate outputs
    If trueFunc cond inputs outputs
      -> If trueFunc cond
        <$> instantiate inputs
        <*> instantiate outputs
    IfElse falseFunc trueFunc cond inputs outputs
      -> IfElse falseFunc trueFunc cond
        <$> instantiate inputs
        <*> instantiate outputs
    Option someFunc cond inputs outputs
      -> Option someFunc cond
        <$> instantiate inputs
        <*> instantiate outputs
    OptionElse noneFunc someFunc cond inputs outputs
      -> OptionElse noneFunc someFunc cond
        <$> instantiate inputs
        <*> instantiate outputs

runInstantiateM
  :: TemplateArguments Template
  -> InstantiateM a
  -> a
runInstantiateM = flip runReader

parameter
  :: TemplateVar
  -> InstantiateM (TemplateArgument Template)
parameter var = do
  parameters <- ask
  case Map.lookup var parameters of
    Nothing -> error $ "Failed to find template argument of var " ++ show var
    Just param -> return param

instantiateRowVar
  :: Var Template
  -> InstantiateM (Vector (Var Normal))
instantiateRowVar = error "TODO instantiateRowVar"

instantiateTemplate
  :: TemplateArguments Template
  -> Function Template
  -> Function Normal
instantiateTemplate arguments
  = runInstantiateM arguments . instantiate
