{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

module Kitten.SSA.FromTyped
  ( fragmentToSSA
  ) where

import Debug.Trace

import Control.Applicative
import Control.Monad (forM, liftM, unless)
import Control.Monad.Trans.Class (lift)
import Control.Monad.Trans.Reader hiding (local)
import Control.Monad.Trans.State
import Control.Monad.Trans.Writer
import Data.Vector (Vector)

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
  , envDataStack :: ![Var Template]
  , envLocalIndex :: !Int
  , envLocalStack :: ![Var Template]
  , envTemplateParameters :: Vector TemplateParameter

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
  , envTemplateParameters = V.empty

  , envParameterIndex = 0
  , envInferredInputArity = 0
  , envInferredOutputArity = 0
  }

type GlobalState = StateT Location (Reader GlobalEnv)
type FunctionState = StateT FunctionEnv GlobalState
type FunctionWriter
  = WriterT (Vector (Instruction Template)) FunctionState

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

funcFailure' :: String -> FunctionState a
funcFailure' message = do
  loc <- lift get
  error $ message ++ " at " ++ show loc

funcFailure :: String -> FunctionWriter a
funcFailure message = do
  loc <- liftGlobalState get
  error $ message ++ " at " ++ show loc

popNormal :: FunctionState (Var Normal)
popNormal = do
  var <- pop
  case downcast var of
    Nothing -> funcFailure' $ "Popped a template variable but expected a normal variable: " ++ show var
    Just v -> return v

pop :: FunctionState (Var Template)
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

freshVarIndex :: FunctionState Int
freshVarIndex = do
  env <- get
  let FunctionEnv{envDataIndex} = env
  put env { envDataIndex = envDataIndex + 1 }
  return envDataIndex

pushNormal :: FunctionState (Var Normal)
pushNormal = do
  index <- freshVarIndex
  let var = Var index Data
  pushVar var
  return var

pushVar :: Var form -> FunctionState ()
pushVar var = modify $ \env -> env
  { envDataStack = upcast var : envDataStack env }

popLocal :: FunctionState (Var Template)
popLocal = do
  env <- get
  case envLocalStack env of
    [] -> lift $ failure "Popping empty locals stack"
    (x:xs) -> do
      put env { envLocalStack = xs }
      return x

getLocal :: Name -> FunctionState (Var Template)
getLocal name = do
  localStack <- gets envLocalStack
  return $ localStack !! nameIndex name

pushLocalVar :: Var Template -> FunctionState ()
pushLocalVar var = modify $ \env -> env
  { envLocalStack = var : envLocalStack env }

-- | Converts a function and its closures to SSA form.
functionToSSA
  :: Typed
  -> Location
  -> GlobalState AFunction
functionToSSA term loc = do
  (instructions, env)
    <- traceShow (Typed.typedType term, loc)
    $ flip runStateT defaultFunctionEnv
    . execWriterT $ do
      (inputArity, outputArity) <- liftState
        $ functionRowArity (Typed.typedType term)
      functionPrelude inputArity
      termToSSA term
      functionEpilogue outputArity

  -- HACK(strager)
  let
    inputs = ScalarArity (envInferredInputArity env)
    outputs = ScalarArity (envInferredOutputArity env)

  return $ simplifyFunctionForm Function
    { funcInputs = inputs
    , funcOutputs = outputs
    , funcInstructions = instructions
    , funcClosures = envClosures env
    , funcTemplateParameters = Parameters (envTemplateParameters env)
    , funcLocation = loc
    }

simplifyFunctionForm :: Function Template -> AFunction
simplifyFunctionForm function@Function{..}
  = case funcTemplateParameters of
    Parameters parameters | V.null parameters
      -> case downcast function of
        Nothing -> error "No parameters for template function"
        Just f -> NormalFunction f
    _ -> TemplateFunction function

functionPrelude :: RowArity form -> FunctionWriter ()
functionPrelude arity = case arity of
  ScalarArity scalars -> liftState $ pushInputs scalars
  TemplateArity templateVar scalars -> do
    rowIndex <- liftState freshVarIndex
    liftState . pushVar $ Var rowIndex (RowVar templateVar)
    liftState $ pushInputs scalars
  where
  pushInputs :: Int -> FunctionState ()
  pushInputs count = mapM_ pushInput [0 .. count - 1]
  pushInput :: Int -> FunctionState ()
  pushInput index = pushVar $ Var index Parameter

functionEpilogue :: RowArity Template -> FunctionWriter ()
functionEpilogue arity = do
  returnValues <- liftState $ popRow arity
  dataStack <- liftState $ gets envDataStack
  unless (null dataStack)
    . error $ "Expected empty stack (did you break the type system?); got " ++ show dataStack
  tellInstruction $ Return returnValues UnknownLocation

setLocation :: Location -> FunctionWriter ()
setLocation = liftGlobalState . put

tellInstruction :: Instruction Template -> FunctionWriter ()
tellInstruction = tell . V.singleton

-- TODO(strager): Find a better name.
tellTemplateParameter
  :: TemplateParameter -> FunctionState TemplateVar
tellTemplateParameter parameter = do
  parameters <- gets envTemplateParameters
  let var = TemplateVar (V.length parameters)
  modify $ \s -> s
    { envTemplateParameters = parameters `V.snoc` parameter }
  return var

liftState :: FunctionState a -> FunctionWriter a
liftState = lift

liftGlobalState :: GlobalState a -> FunctionWriter a
liftGlobalState = lift . lift

popRow :: RowArity Template -> FunctionState (RowVar Template)
popRow = \case
  ScalarArity scalars -> ScalarVars <$> popScalars scalars
  TemplateArity _templateVar scalars -> do
    -- TODO(strager): Assert here that 'rowVar' is actually a
    -- 'RowVar' with the same template variable.
    scalarVars <- popScalars scalars
    rowVar <- pop
    return $ TemplateRowScalarVars rowVar scalarVars
  where
  popScalars :: Int -> FunctionState (Vector (Var Normal))
  popScalars count = V.replicateM count popNormal

pushRow :: RowArity Template -> FunctionState (RowVar Template)
pushRow = \case
  ScalarArity scalars -> ScalarVars <$> pushScalars scalars
  TemplateArity templateVar scalars -> do
    rowIndex <- freshVarIndex
    let rowVar = Var rowIndex (RowVar templateVar)
    pushVar rowVar
    scalarVars <- pushScalars scalars
    return $ TemplateRowScalarVars rowVar scalarVars
  where
  pushScalars :: Int -> FunctionState (Vector (Var Normal))
  pushScalars count = V.replicateM count pushNormal

-- | FIXME(strager): Better description.  Returns the number
-- of values consumed and returned by a function with the
-- given type.
functionRowArity
  :: Type Type.Scalar
  -> FunctionState (RowArity Template, RowArity Template)
functionRowArity = \case
  Type.Function r s _effect _loc
    | rBottommost == sBottommost
    -> return (ScalarArity (Type.rowDepth r), ScalarArity (Type.rowDepth s))
    | otherwise -> do
      rVar <- tellTemplateParameter $ RowParam (rowVar rBottommost)
      sVar <- tellTemplateParameter $ RowParam (rowVar sBottommost)
      return
        ( TemplateArity rVar (Type.rowDepth r)
        , TemplateArity sVar (Type.rowDepth s)
        )
    where
    rBottommost, sBottommost :: Type Type.Row
    rBottommost = Type.bottommost r
    sBottommost = Type.bottommost s
    rowVar :: Type Type.Row -> Type.TypeName Type.Row
    rowVar (Type.Var var _loc) = var
    rowVar type_ = error $ "Not a row variable: " ++ show type_
  type_ -> error $ "Not a function type: " ++ show type_

termToSSA :: Typed -> FunctionWriter ()
termToSSA theTerm = setLocation UnknownLocation{- FIXME -} >> case theTerm of
  Typed.Builtin builtin loc type_ -> builtinToSSA builtin loc type_
  Typed.Call name loc _type -> do
    fn <- liftGlobalState $ lookUpFunction name
    {-
    -- HACK(strager): _type has superfluous scalars on
    -- either side of the function type, throwing off
    -- functionArity.  We therefore look up the type from
    -- the function's definition.
    let type_ = Typed.typedType (Type.unScheme (defTerm fn))
    -}
    let type_ = _type

    traceShow type_ (return())
    (inArity, outArity) <- liftState $ functionRowArity type_
    inputs <- liftState $ popRow inArity
    outputs <- liftState $ pushRow outArity
    let funcName = GlobalFunctionName (defName fn)
    tellInstruction $ Call funcName inputs outputs loc
  Typed.Compose terms _loc _type -> V.mapM_ termToSSA terms
  Typed.From{} -> return ()
  Typed.PairTerm a b loc _type -> do
    aVar <- termToSSA a >> liftState popNormal
    bVar <- termToSSA b >> liftState popNormal
    pairVar <- liftState pushNormal
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
    vars <- V.mapM (\v -> termToSSA v >> liftState popNormal) values
    var <- liftState pushNormal
    tellInstruction $ Vector vars var loc

builtinToSSA
  :: Builtin
  -> Location
  -> Type Type.Scalar
  -> FunctionWriter ()
builtinToSSA theBuiltin loc type_ = do
  mbuiltin <- getMaybeBuiltin $ case theBuiltin of
    Builtin.AddFloat       -> Just $ AddFloat       <$> popNormal <*> popNormal <*> pushNormal
    Builtin.AddInt         -> Just $ AddInt         <$> popNormal <*> popNormal <*> pushNormal
    Builtin.AddVector      -> Just $ AddVector      <$> popNormal <*> popNormal <*> pushNormal
    Builtin.AndBool        -> Just $ AndBool        <$> popNormal <*> popNormal <*> pushNormal
    Builtin.AndInt         -> Just $ AndInt         <$> popNormal <*> popNormal <*> pushNormal
    Builtin.CharToInt      -> Just $ CharToInt      <$> popNormal <*> pushNormal
    Builtin.Close          -> Just $ Close          <$> popNormal
    Builtin.DivFloat       -> Just $ DivFloat       <$> popNormal <*> popNormal <*> pushNormal
    Builtin.DivInt         -> Just $ DivInt         <$> popNormal <*> popNormal <*> pushNormal
    Builtin.EqFloat        -> Just $ EqFloat        <$> popNormal <*> popNormal <*> pushNormal
    Builtin.EqInt          -> Just $ EqInt          <$> popNormal <*> popNormal <*> pushNormal
    Builtin.Exit           -> Just $ Exit           <$> popNormal
    Builtin.First          -> Just $ First          <$> popNormal <*> pushNormal
    Builtin.FromLeft       -> Just $ FromLeft       <$> popNormal <*> pushNormal
    Builtin.FromRight      -> Just $ FromRight      <$> popNormal <*> pushNormal
    Builtin.FromSome       -> Just $ FromSome       <$> popNormal <*> pushNormal
    Builtin.GeFloat        -> Just $ GeFloat        <$> popNormal <*> popNormal <*> pushNormal
    Builtin.GeInt          -> Just $ GeInt          <$> popNormal <*> popNormal <*> pushNormal
    Builtin.Get            -> Just $ Get            <$> popNormal <*> popNormal <*> pushNormal
    Builtin.GetLine        -> Just $ GetLine        <$> popNormal <*> pushNormal
    Builtin.GtFloat        -> Just $ GtFloat        <$> popNormal <*> popNormal <*> pushNormal
    Builtin.GtInt          -> Just $ GtInt          <$> popNormal <*> popNormal <*> pushNormal
    Builtin.Impure         -> Nothing
    Builtin.Init           -> Just $ Init           <$> popNormal <*> pushNormal
    Builtin.IntToChar      -> Just $ IntToChar      <$> popNormal <*> pushNormal
    Builtin.LeFloat        -> Just $ LeFloat        <$> popNormal <*> popNormal <*> pushNormal
    Builtin.LeInt          -> Just $ LeInt          <$> popNormal <*> popNormal <*> pushNormal
    Builtin.Left           -> Just $ MakeLeft       <$> popNormal <*> pushNormal
    Builtin.Length         -> Just $ Length         <$> popNormal <*> pushNormal
    Builtin.LtFloat        -> Just $ LtFloat        <$> popNormal <*> popNormal <*> pushNormal
    Builtin.LtInt          -> Just $ LtInt          <$> popNormal <*> popNormal <*> pushNormal
    Builtin.ModFloat       -> Just $ ModFloat       <$> popNormal <*> popNormal <*> pushNormal
    Builtin.ModInt         -> Just $ ModInt         <$> popNormal <*> popNormal <*> pushNormal
    Builtin.MulFloat       -> Just $ MulFloat       <$> popNormal <*> popNormal <*> pushNormal
    Builtin.MulInt         -> Just $ MulInt         <$> popNormal <*> popNormal <*> pushNormal
    Builtin.NeFloat        -> Just $ NeFloat        <$> popNormal <*> popNormal <*> pushNormal
    Builtin.NeInt          -> Just $ NeInt          <$> popNormal <*> popNormal <*> pushNormal
    Builtin.NegFloat       -> Just $ NegFloat       <$> popNormal <*> pushNormal
    Builtin.NegInt         -> Just $ NegInt         <$> popNormal <*> pushNormal
    Builtin.None           -> Just $ None           <$> pushNormal
    Builtin.NotBool        -> Just $ NotBool        <$> popNormal <*> pushNormal
    Builtin.NotInt         -> Just $ NotInt         <$> popNormal <*> pushNormal
    Builtin.OpenIn         -> Just $ OpenIn         <$> pushNormal
    Builtin.OpenOut        -> Just $ OpenOut        <$> pushNormal
    Builtin.OrBool         -> Just $ OrBool         <$> popNormal <*> popNormal <*> pushNormal
    Builtin.OrInt          -> Just $ OrInt          <$> popNormal <*> popNormal <*> pushNormal
    Builtin.Pair           -> Just $ Pair           <$> popNormal <*> popNormal <*> pushNormal
    Builtin.Print          -> Just $ Print          <$> popNormal <*> popNormal
    Builtin.Rest           -> Just $ Rest           <$> popNormal <*> pushNormal
    Builtin.Right          -> Just $ MakeRight      <$> popNormal <*> pushNormal
    Builtin.Set            -> Just $ Set            <$> popNormal <*> popNormal <*> popNormal <*> pushNormal
    Builtin.ShowFloat      -> Just $ ShowFloat      <$> popNormal <*> pushNormal
    Builtin.ShowInt        -> Just $ ShowInt        <$> popNormal <*> pushNormal
    Builtin.Some           -> Just $ Some           <$> popNormal <*> pushNormal
    Builtin.Stderr         -> Just $ Stderr         <$> pushNormal
    Builtin.Stdin          -> Just $ Stdin          <$> pushNormal
    Builtin.Stdout         -> Just $ Stdout         <$> pushNormal
    Builtin.SubFloat       -> Just $ SubFloat       <$> popNormal <*> popNormal <*> pushNormal
    Builtin.SubInt         -> Just $ SubInt         <$> popNormal <*> popNormal <*> pushNormal
    Builtin.Tail           -> Just $ Tail           <$> popNormal <*> pushNormal
    Builtin.UnsafePurify11 -> Just $ UnsafePurify11 <$> popNormal <*> pushNormal
    Builtin.XorBool        -> Just $ XorBool        <$> popNormal <*> popNormal <*> pushNormal
    Builtin.XorInt         -> Just $ XorInt         <$> popNormal <*> popNormal <*> pushNormal
    Builtin.Apply -> Just . rowPolymorphic 0 $ Apply
      <$> popNormal
    Builtin.Choice -> Just . rowPolymorphic 1 $ Choice
      <$> popNormal
      <*> popNormal
    Builtin.ChoiceElse -> Just . rowPolymorphic 1 $ ChoiceElse
      <$> popNormal
      <*> popNormal
      <*> popNormal
    Builtin.If -> Just . rowPolymorphic 0 $ If
      <$> popNormal
      <*> popNormal
    Builtin.IfElse -> Just . rowPolymorphic 0 $ IfElse
      <$> popNormal
      <*> popNormal
      <*> popNormal
    Builtin.Option -> Just . rowPolymorphic 1 $ Option
      <$> popNormal
      <*> popNormal
    Builtin.OptionElse -> Just . rowPolymorphic 0 $ OptionElse
      <$> popNormal
      <*> popNormal
      <*> popNormal
  case mbuiltin of
    Nothing -> return ()
    Just builtin -> tellInstruction $ CallBuiltin builtin loc
  where
  rowPolymorphic
    :: Int
    -> (FunctionState (RowVar Template -> RowVar Template -> a))
    -> FunctionState a
  -- TODO(strager): Use _argsToDrop.
  rowPolymorphic _argsToDrop constructor = do
    (inputs, outputs) <- case type_ of
      Type.Function (_ Type.:. functionType) _rhs _effect _loc
        -> functionRowArity functionType
      _ -> err $ "failed to find function argument for builtin " ++ show theBuiltin ++ " in type " ++ show type_
    constructor <*> popRow inputs <*> pushRow outputs

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
    var <- liftState pushNormal
    tellInstruction $ Activation name closedVars var loc
  Typed.Float value -> one $ Float value
  Typed.Int value -> one $ Int value
  Typed.Local name -> do
    local <- liftState $ getLocal name
    liftState $ pushVar local
  Typed.String value -> do
    charVars <- mapM (\c -> one (Char c) >> liftState popNormal)
      $ Text.unpack value
    one $ Vector (V.fromList charVars)

  Typed.Unit -> removeFromASTPlease "Unit"
  where
  one
    :: (Var Normal -> Location -> Instruction Template)
    -> FunctionWriter ()
  one f = do
    var <- liftState pushNormal
    tellInstruction $ f var loc

  -- | FIXME(strager): Several Typed.Value AST
  -- constuctors should be removed, as they are only need
  -- for the interpreter.
  removeFromASTPlease :: String -> FunctionWriter ()
  removeFromASTPlease ctorName = funcFailure
    $ "Kitten.SSA.valueToSSA: remove Typed.Value(" ++ ctorName ++ ")"
