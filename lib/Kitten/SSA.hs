{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

module Kitten.SSA
  ( fragmentToSSA
  ) where

import Control.Applicative
import Control.Monad (forM, liftM, unless)
import Control.Monad.Trans.Class (lift)
import Control.Monad.Trans.Reader hiding (local)
import Control.Monad.Trans.State
import Control.Monad.Trans.Writer
import Data.Maybe (fromMaybe)
import Data.Vector (Vector)
import Unsafe.Coerce (unsafeCoerce)

import qualified Data.Vector as V

import Kitten.Builtin (Builtin)
import Kitten.ClosedName
import Kitten.Def
import Kitten.Fragment
import Kitten.Location (Location(UnknownLocation))
import Kitten.Name
import Kitten.SSA.Types
import Kitten.Type (Type)
import Kitten.Typed (Typed, TypedDef)

import qualified Kitten.Builtin as Builtin
import qualified Kitten.Kind as Type
import qualified Kitten.Type as Type
import qualified Kitten.Typed as Typed
import qualified Kitten.Util.Text as Text

data GlobalEnv = GlobalEnv
  { envDefs :: !(Vector TypedDef)
  }

data FunctionEnv = FunctionEnv
  { envClosures :: !(Vector Closure)
  , envDataIndex :: !Int
  , envDataStack :: ![Var]
  , envLocalIndex :: !Int
  , envLocalStack :: ![Var]

  -- HACK(strager)
  , envParameterIndex :: !Int
  , envInferredInputArity :: !Int
  , envInferredOutputArity :: !Int
  }

defaultFunctionEnv :: FunctionEnv
defaultFunctionEnv = FunctionEnv
  { envClosures = V.empty
  , envDataIndex = 0
  , envDataStack = []
  , envLocalIndex = 0
  , envLocalStack = []

  , envParameterIndex = 0
  , envInferredInputArity = 0
  , envInferredOutputArity = 0
  }

type GlobalState = StateT Location (Reader GlobalEnv)
type FunctionState = StateT FunctionEnv GlobalState
type FunctionWriter
  = WriterT (Vector (Instruction Template))
    (WriterT (Vector TemplateParameter)
      FunctionState)

fragmentToSSA
  :: Fragment Typed
  -> (AFunction, [ADefinition])
fragmentToSSA Fragment{fragmentDefs, fragmentTerms}
  = flip runReader GlobalEnv { envDefs = fragmentDefs }
  . flip evalStateT UnknownLocation $ do
    fragmentSSA <- functionToSSA
      (V.head fragmentTerms)  -- FIXME(strager)
      UnknownLocation  -- FIXME(strager)
    definitionSSAs <- forM (V.toList fragmentDefs) $ \def -> do
      let
        loc = defLocation def
        term = Type.unScheme (defTerm def)
      ssa <- functionToSSA term loc
      let name = GlobalFunctionName (defName def)
      return $ case ssa of
        NormalFunction f -> NormalDefinition Definition
          { definitionName = name
          , definitionFunction = f
          }
        TemplateFunction f -> TemplateDefinition Definition
          { definitionName = name
          , definitionFunction = f
          }
    return (fragmentSSA, definitionSSAs)

lookUpFunction :: Name -> GlobalState TypedDef
lookUpFunction name = do
  defs <- lift $ asks envDefs
  return $ defs V.! nameIndex name

failure :: String -> GlobalState a
failure message = do
  loc <- get
  error $ message ++ " at " ++ show loc

funcFailure :: String -> FunctionWriter a
funcFailure message = do
  loc <- liftGlobalState get
  error $ message ++ " at " ++ show loc

pop :: FunctionState Var
pop = do
  env <- get
  case envDataStack env of
    (x:xs) -> do
      put env { envDataStack = xs }
      return x
    -- FIXME(strager): Instead of popping locals here, this
    -- should be an error, and locals should be pushed in a
    -- function prelude.  However, we don't have type
    -- information for closures, so we don't know how many
    -- things to pop off the stack.
    [] -> {-lift $ failure "Popping empty stack"-} do
      let param = envParameterIndex env
      put env
        { envParameterIndex = param + 1
        , envInferredInputArity = envInferredInputArity env + 1
        }
      return (Var param Parameter)

push :: FunctionState Var
push = do
  env <- get
  let FunctionEnv{envDataIndex, envDataStack} = env
  let var = Var envDataIndex Data
  put env
    { envDataIndex = envDataIndex + 1
    , envDataStack = var : envDataStack
    }
  return var

pushVar :: Var -> FunctionState ()
pushVar var = modify $ \env -> env
  { envDataStack = var : envDataStack env }

popLocal :: FunctionState Var
popLocal = do
  env <- get
  case envLocalStack env of
    [] -> lift $ failure "Popping empty locals stack"
    (x:xs) -> do
      put env { envLocalStack = xs }
      return x

getLocal :: Name -> FunctionState Var
getLocal name = do
  localStack <- gets envLocalStack
  return $ localStack !! nameIndex name

pushLocalVar :: Var -> FunctionState ()
pushLocalVar var = modify $ \env -> env
  { envLocalStack = var : envLocalStack env }

-- | Converts a function and its closures to SSA form.
functionToSSA
  :: Typed
  -> Location
  -> GlobalState AFunction
functionToSSA term loc = do
  ((instructions, templateParameters), env)
    <- flip runStateT defaultFunctionEnv
    . runWriterT . execWriterT $ do
      functionPrelude 0  -- FIXME(strager)
      termToSSA term
      functionEpilogue 0  -- FIXME(strager)

  -- HACK(strager)
  let
    inputs = ScalarArity (envInferredInputArity env)
    outputs = ScalarArity (envInferredOutputArity env)

  return $ simplifyFunctionForm Function
    { funcInputs = inputs
    , funcOutputs = outputs
    , funcInstructions = instructions
    , funcClosures = envClosures env
    , funcTemplateParameters = Parameters templateParameters
    , funcLocation = loc
    }

simplifyFunctionForm :: Function Template -> AFunction
simplifyFunctionForm function@Function{..}
  = case funcTemplateParameters of
    Parameters parameters | V.null parameters
      -> NormalFunction $ castFunction function
    _ -> TemplateFunction function

castFunction :: Function Template -> Function Normal
castFunction Function{..} = Function
  -- NOTE(strager): GHC does not support type-changing GADT
  -- record updates.
  { funcInputs = castRowArity funcInputs
  , funcOutputs = castRowArity funcOutputs
  , funcInstructions = castInstructions funcInstructions
  , funcClosures = funcClosures
  , funcTemplateParameters = NoParameters
  , funcLocation = funcLocation
  }

castRowArity :: RowArity Template -> RowArity Normal
castRowArity = \case
  ScalarArity arity -> ScalarArity arity
  TemplateArity{} -> error "Kitten.SSA.castRowArity: Found TemplateArity"

castInstructions
  :: Vector (Instruction Template)
  -> Vector (Instruction Normal)
-- FIXME(strager): Should throw a runtime error upon failure.
castInstructions = unsafeCoerce

-- TODO(strager): Remove hacks in 'pop' and use this instead.
functionPrelude :: Int -> FunctionWriter ()
functionPrelude _arity = return ()

-- TODO(strager): Use arity instead of 'pop'-like hacks.
functionEpilogue :: Int -> FunctionWriter ()
functionEpilogue _arity = do
  env <- lift $ lift get
  let returnValues = envDataStack env
  lift . lift $ put env
    { envDataStack = []
    , envInferredOutputArity = length returnValues
    }
  tellInstruction $ Return
    (V.reverse $ V.fromList returnValues)
    UnknownLocation

setLocation :: Location -> FunctionWriter ()
setLocation = liftGlobalState . put

tellInstruction :: Instruction Template -> FunctionWriter ()
tellInstruction = tell . V.singleton

liftState :: FunctionState a -> FunctionWriter a
liftState = lift . lift

liftGlobalState :: GlobalState a -> FunctionWriter a
liftGlobalState = lift . lift . lift

termToSSA :: Typed -> FunctionWriter ()
termToSSA theTerm = setLocation UnknownLocation{- FIXME -} >> case theTerm of
  Typed.Builtin builtin loc type_ -> builtinToSSA builtin loc type_
  Typed.Call name loc _type -> do
    -- HACK(strager): _type has supliferous scalars on
    -- either side of the function type, throwing off
    -- functionArity.  We therefore look up the type from
    -- the function's definition.
    fn <- lift . lift . lift $ lookUpFunction name
    let type_ = Typed.typedType (Type.unScheme (defTerm fn))

    -- TODO(strager): Allow polymorphic arity.
    case Type.functionArity type_ of
      Nothing -> funcFailure
        $ "Could not get arity of function " ++ show fn
      Just (inArity, outArity) -> do
        inputs <- lift . lift $ V.replicateM inArity pop
        outputs <- lift . lift $ V.replicateM outArity push
        let funcName = GlobalFunctionName (defName fn)
        tellInstruction $ Call funcName
          (ScalarVars (V.reverse inputs))
          (ScalarVars outputs)
          loc
  Typed.Compose terms _loc _type -> V.mapM_ termToSSA terms
  Typed.From{} -> return ()
  Typed.PairTerm a b loc _type -> do
    aVar <- termToSSA a >> liftState pop
    bVar <- termToSSA b >> liftState pop
    pairVar <- liftState push
    tellInstruction $ PairTerm aVar bVar pairVar loc
  Typed.Push value loc _type -> valueToSSA value loc
  Typed.Scoped term _loc _type -> do
    var <- liftState pop
    liftState $ pushLocalVar var
    termToSSA term
    poppedVar <- liftState popLocal
    unless (var == poppedVar)
      . funcFailure
      $ "Local mismatch (pushed " ++ show var ++ " vs popped " ++ show poppedVar ++ ")"
  Typed.To{} -> return ()
  Typed.VectorTerm values loc _type -> do
    vars <- V.mapM (\v -> termToSSA v >> liftState pop) values
    var <- liftState push
    tellInstruction $ Vector vars var loc

builtinToSSA
  :: Builtin
  -> Location
  -> Type Type.Scalar
  -> FunctionWriter ()
builtinToSSA theBuiltin loc type_ = do
  mbuiltin <- getMaybeBuiltin $ case theBuiltin of
    Builtin.AddFloat       -> Just $ AddFloat       <$> pop <*> pop <*> push
    Builtin.AddInt         -> Just $ AddInt         <$> pop <*> pop <*> push
    Builtin.AddVector      -> Just $ AddVector      <$> pop <*> pop <*> push
    Builtin.AndBool        -> Just $ AndBool        <$> pop <*> pop <*> push
    Builtin.AndInt         -> Just $ AndInt         <$> pop <*> pop <*> push
    Builtin.CharToInt      -> Just $ CharToInt      <$> pop <*> push
    Builtin.Close          -> Just $ Close          <$> pop
    Builtin.DivFloat       -> Just $ DivFloat       <$> pop <*> pop <*> push
    Builtin.DivInt         -> Just $ DivInt         <$> pop <*> pop <*> push
    Builtin.EqFloat        -> Just $ EqFloat        <$> pop <*> pop <*> push
    Builtin.EqInt          -> Just $ EqInt          <$> pop <*> pop <*> push
    Builtin.Exit           -> Just $ Exit           <$> pop
    Builtin.First          -> Just $ First          <$> pop <*> push
    Builtin.FromLeft       -> Just $ FromLeft       <$> pop <*> push
    Builtin.FromRight      -> Just $ FromRight      <$> pop <*> push
    Builtin.FromSome       -> Just $ FromSome       <$> pop <*> push
    Builtin.GeFloat        -> Just $ GeFloat        <$> pop <*> pop <*> push
    Builtin.GeInt          -> Just $ GeInt          <$> pop <*> pop <*> push
    Builtin.Get            -> Just $ Get            <$> pop <*> pop <*> push
    Builtin.GetLine        -> Just $ GetLine        <$> pop <*> push
    Builtin.GtFloat        -> Just $ GtFloat        <$> pop <*> pop <*> push
    Builtin.GtInt          -> Just $ GtInt          <$> pop <*> pop <*> push
    Builtin.Impure         -> Nothing
    Builtin.Init           -> Just $ Init           <$> pop <*> push
    Builtin.IntToChar      -> Just $ IntToChar      <$> pop <*> push
    Builtin.LeFloat        -> Just $ LeFloat        <$> pop <*> pop <*> push
    Builtin.LeInt          -> Just $ LeInt          <$> pop <*> pop <*> push
    Builtin.Left           -> Just $ MakeLeft       <$> pop <*> push
    Builtin.Length         -> Just $ Length         <$> pop <*> push
    Builtin.LtFloat        -> Just $ LtFloat        <$> pop <*> pop <*> push
    Builtin.LtInt          -> Just $ LtInt          <$> pop <*> pop <*> push
    Builtin.ModFloat       -> Just $ ModFloat       <$> pop <*> pop <*> push
    Builtin.ModInt         -> Just $ ModInt         <$> pop <*> pop <*> push
    Builtin.MulFloat       -> Just $ MulFloat       <$> pop <*> pop <*> push
    Builtin.MulInt         -> Just $ MulInt         <$> pop <*> pop <*> push
    Builtin.NeFloat        -> Just $ NeFloat        <$> pop <*> pop <*> push
    Builtin.NeInt          -> Just $ NeInt          <$> pop <*> pop <*> push
    Builtin.NegFloat       -> Just $ NegFloat       <$> pop <*> push
    Builtin.NegInt         -> Just $ NegInt         <$> pop <*> push
    Builtin.None           -> Just $ None           <$> push
    Builtin.NotBool        -> Just $ NotBool        <$> pop <*> push
    Builtin.NotInt         -> Just $ NotInt         <$> pop <*> push
    Builtin.OpenIn         -> Just $ OpenIn         <$> push
    Builtin.OpenOut        -> Just $ OpenOut        <$> push
    Builtin.OrBool         -> Just $ OrBool         <$> pop <*> pop <*> push
    Builtin.OrInt          -> Just $ OrInt          <$> pop <*> pop <*> push
    Builtin.Pair           -> Just $ Pair           <$> pop <*> pop <*> push
    Builtin.Print          -> Just $ Print          <$> pop <*> pop
    Builtin.Rest           -> Just $ Rest           <$> pop <*> push
    Builtin.Right          -> Just $ MakeRight      <$> pop <*> push
    Builtin.Set            -> Just $ Set            <$> pop <*> pop <*> pop <*> push
    Builtin.ShowFloat      -> Just $ ShowFloat      <$> pop <*> push
    Builtin.ShowInt        -> Just $ ShowInt        <$> pop <*> push
    Builtin.Some           -> Just $ Some           <$> pop <*> push
    Builtin.Stderr         -> Just $ Stderr         <$> push
    Builtin.Stdin          -> Just $ Stdin          <$> push
    Builtin.Stdout         -> Just $ Stdout         <$> push
    Builtin.SubFloat       -> Just $ SubFloat       <$> pop <*> pop <*> push
    Builtin.SubInt         -> Just $ SubInt         <$> pop <*> pop <*> push
    Builtin.Tail           -> Just $ Tail           <$> pop <*> push
    Builtin.UnsafePurify11 -> Just $ UnsafePurify11 <$> pop <*> push
    Builtin.XorBool        -> Just $ XorBool        <$> pop <*> pop <*> push
    Builtin.XorInt         -> Just $ XorInt         <$> pop <*> pop <*> push
    -- TODO(strager): Allow polymorphic builtins.
    Builtin.Apply -> Just $ Apply
      <$> pop
      <*> pushInputs 0
      <*> pushOutputs
    Builtin.Choice -> Just $ Choice
      <$> pop
      <*> pop
      <*> pushInputs 1
      <*> pushOutputs
    Builtin.ChoiceElse -> Just $ ChoiceElse
      <$> pop
      <*> pop
      <*> pop
      <*> pushInputs 1
      <*> pushOutputs
    Builtin.If -> Just $ If
      <$> pop
      <*> pop
      <*> pushInputs 0
      <*> pushOutputs
    Builtin.IfElse -> Just $ IfElse
      <$> pop
      <*> pop
      <*> pop
      <*> pushInputs 0
      <*> pushOutputs
    Builtin.Option -> Just $ Option
      <$> pop
      <*> pop
      <*> pushInputs 1
      <*> pushOutputs
    Builtin.OptionElse -> Just $ OptionElse
      <$> pop
      <*> pop
      <*> pop
      <*> pushInputs 0
      <*> pushOutputs
  case mbuiltin of
    Nothing -> return ()
    Just builtin -> tellInstruction $ CallBuiltin builtin loc
  where
  inputs, outputs :: Int
  ~(~inputs, ~outputs) = case type_ of
    Type.Function (_ Type.:. functionType) _rhs _effect _loc
      -> fromMaybe (err $ "failed to get arity of type " ++ show functionType)
        $ Type.functionArity functionType
    _ -> err $ "failed to find function argument for builtin " ++ show theBuiltin ++ " in type " ++ show type_

  -- TODO(strager): Allow RowTemplateVar.
  pushInputs :: Int -> FunctionState (RowVar Template)
  pushInputs argsToDrop
    = ScalarVars <$> V.replicateM (inputs - argsToDrop) pop
  pushOutputs :: FunctionState (RowVar Template)
  pushOutputs = ScalarVars <$> V.replicateM outputs push

  err :: String -> a
  err m = error $ "Kitten.SSA.builtinToSSA: " ++ m
  getMaybeBuiltin :: Maybe (FunctionState a) -> FunctionWriter (Maybe a)
  getMaybeBuiltin = liftState . maybe (return Nothing) (liftM Just)

addClosure :: Closure -> FunctionState ClosureName
addClosure closure = do
  index <- liftM V.length $ gets envClosures
  modify $ \env -> env { envClosures = envClosures env `V.snoc` closure }
  return $ ClosureName index

valueToSSA
  :: Typed.Value
  -> Location
  -> FunctionWriter ()
valueToSSA theValue loc = case theValue of
  Typed.Bool value -> one $ Bool value
  Typed.Char value -> one $ Char value
  Typed.Closed name -> liftState . pushVar
    $ Var (nameIndex name) Closed
  Typed.Closure closedNames term -> do
    closedVars <- V.forM closedNames $ \closed -> case closed of
      ClosedName name -> liftState $ getLocal name
      ReclosedName name -> return $ Var (nameIndex name) Closed
    func <- liftGlobalState $ functionToSSA term loc
    let
      closure = Closure
        { closureClosed = V.length closedVars
        , closureFunction = func
        }
    name <- liftState $ addClosure closure
    var <- liftState push
    tellInstruction $ Activation name closedVars var loc
  Typed.Float value -> one $ Float value
  Typed.Int value -> one $ Int value
  Typed.Local name -> do
    local <- liftState $ getLocal name
    liftState $ pushVar local
  Typed.String value -> do
    charVars <- mapM (\c -> one (Char c) >> liftState pop)
      $ Text.unpack value
    one $ Vector (V.fromList charVars)

  Typed.Unit -> removeFromASTPlease "Unit"
  where
  one
    :: (Var -> Location -> Instruction Template)
    -> FunctionWriter ()
  one f = do
    var <- liftState push
    tellInstruction $ f var loc

  -- | FIXME(strager): Several Typed.Value AST
  -- constuctors should be removed, as they are only need
  -- for the interpreter.
  removeFromASTPlease :: String -> FunctionWriter ()
  removeFromASTPlease ctorName = funcFailure
    $ "Kitten.SSA.valueToSSA: remove Typed.Value(" ++ ctorName ++ ")"
