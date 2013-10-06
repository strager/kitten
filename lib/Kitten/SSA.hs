{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

module Kitten.SSA
  ( BuiltinCall(..)
  , Closure(..)
  , ClosureName(..)
  , Definition(..)
  , Function(..)
  , GlobalFunctionName(..)
  , Instruction(..)
  , Var(..)
  , VarType(..)

  , fragmentToSSA
  , functionToText
  ) where

import Control.Applicative
import Control.Monad (forM, liftM, unless)
import Control.Monad.Trans.Class (lift)
import Control.Monad.Trans.Reader hiding (local)
import Control.Monad.Trans.State
import Control.Monad.Trans.Writer
import Data.Maybe (fromMaybe)
import Data.Monoid ((<>))
import Data.Text (Text)
import Data.Vector (Vector)

import qualified Data.Vector as V

import Kitten.Builtin (Builtin)
import Kitten.ClosedName
import Kitten.Def
import Kitten.Fragment
import Kitten.Location (Location(UnknownLocation))
import Kitten.Name
import Kitten.Type (Type)
import Kitten.Typed (Typed, TypedDef)
import Kitten.Util.Text (ToText(..), showText)

import qualified Kitten.Builtin as Builtin
import qualified Kitten.Type as Type
import qualified Kitten.Typed as Typed
import qualified Kitten.Util.Text as Text

data Function = Function
  { funcInputs :: !Int
  , funcOutputs :: !Int
  , funcInstructions :: !(Vector Instruction)
  , funcClosures :: !(Vector Closure)
  , funcLocation :: !Location
  }

data Definition = Definition
  { definitionName :: !GlobalFunctionName
  , definitionFunction :: !Function
  }

data Closure = Closure
  { closureClosed :: !Int
  -- ^ Number of variables capture by this closure.
  , closureFunction :: !Function
  }

instance Show Function where
  show = Text.unpack . toText

mapVector :: (a -> b) -> Vector a -> [b]
mapVector f = map f . V.toList

vectorToTextList :: (ToText a) => Vector a -> [Text]
vectorToTextList = mapVector toText

unwordsVector :: (ToText a) => Vector a -> Text
unwordsVector = Text.unwords . map toText . V.toList

vectorToLines :: (ToText a) => Vector a -> Text
vectorToLines = Text.unlines . vectorToTextList

instance ToText Function where
  toText = functionToText "<unknown>"

functionToText :: Text -> Function -> Text
functionToText name Function{..} = Text.unlines
  [ Text.concat
    [ name
    , " ("
    , showText funcInputs
    , " -> "
    , showText funcOutputs
    , "):"
    ]
  , Text.indent $ vectorToLines funcInstructions
  , Text.indent . Text.unlines . V.toList $ V.imap
    (\index closure -> functionToText
      (toText (ClosureName index)) (closureFunction closure))
    funcClosures
  ]

-- | An SSA instruction.  Arguments are ordered input first,
-- output last, except for 'Return'.
data Instruction
  = Activation !ClosureName !(Vector Var) !Var !Location
  | Bool !Bool !Var !Location
  | Char !Char !Var !Location
  | Call !GlobalFunctionName !(Vector Var) !(Vector Var) !Location
  | CallBuiltin !BuiltinCall !Location
  | Float !Double !Var !Location
  | Int !Int !Var !Location
  | PairTerm !Var !Var !Var !Location
  | Return !(Vector Var) !Location
  | Vector !(Vector Var) !Var !Location

instance Show Instruction where
  show = Text.unpack . toText

instance ToText Instruction where
  toText instruction = case instruction of
    Activation funcName closed out _ -> Text.unwords
      [ bind out ["act", toText funcName]
      , unwordsVector closed
      ]
    Bool value out _ -> bind out
      ["bool", if value then "true" else "false"]
    Char value out _ -> bind out ["char", showText value]
    Call funcName params outs _ -> Text.unwords
      [ unwordsVector outs
      , "<-", "call", toText funcName
      , unwordsVector params
      ]
    CallBuiltin builtin _ -> toText builtin
    Float value out _ -> bind out ["float", showText value]
    Int value out _ -> bind out ["int", showText value]
    PairTerm a b out _ -> bind out ["pair", showText a, showText b]
    Return values _ -> "return " <> unwordsVector values
    Vector values out _ -> Text.unwords
      [ bind out ["vector"]
      , unwordsVector values
      ]

data BuiltinCall
  = AddFloat !Var !Var !Var
  | AddInt !Var !Var !Var
  | AddVector !Var !Var !Var
  | AndBool !Var !Var !Var
  | AndInt !Var !Var !Var
  | Apply
    !Var           -- ^ Activation.
    !(Vector Var)  -- ^ Inputs.
    !(Vector Var)  -- ^ Outputs.
  | CharToInt !Var !Var
  | Choice
    !Var           -- ^ Left activation.
    !Var           -- ^ Condition.
    !(Vector Var)  -- ^ Extra inputs.
    !(Vector Var)  -- ^ Extra outputs.
  | ChoiceElse
    !Var           -- ^ Right activation.
    !Var           -- ^ Left activation.
    !Var           -- ^ Condition.
    !(Vector Var)  -- ^ Extra inputs.
    !(Vector Var)  -- ^ Extra outputs.
  | Close !Var
  | DivFloat !Var !Var !Var
  | DivInt !Var !Var !Var
  | EqFloat !Var !Var !Var
  | EqInt !Var !Var !Var
  | Exit !Var
  | First !Var !Var
  | FromLeft !Var !Var
  | FromRight !Var !Var
  | FromSome !Var !Var
  | GeFloat !Var !Var !Var
  | GeInt !Var !Var !Var
  | Get !Var !Var !Var
  | GetLine !Var !Var
  | GtFloat !Var !Var !Var
  | GtInt !Var !Var !Var
  | If
    !Var           -- ^ Truthy activation.
    !Var           -- ^ Condition.
    !(Vector Var)  -- ^ Extra inputs.
    !(Vector Var)  -- ^ Extra outputs.
  | IfElse
    !Var           -- ^ Falsy activation.
    !Var           -- ^ Truthy activation.
    !Var           -- ^ Condition.
    !(Vector Var)  -- ^ Extra inputs.
    !(Vector Var)  -- ^ Extra outputs.
  | Init !Var !Var
  | IntToChar !Var !Var
  | LeFloat !Var !Var !Var
  | LeInt !Var !Var !Var
  | Length !Var !Var
  | LtFloat !Var !Var !Var
  | LtInt !Var !Var !Var
  | MakeLeft !Var !Var
  | MakeRight !Var !Var
  | ModFloat !Var !Var !Var
  | ModInt !Var !Var !Var
  | MulFloat !Var !Var !Var
  | MulInt !Var !Var !Var
  | NeFloat !Var !Var !Var
  | NeInt !Var !Var !Var
  | NegFloat !Var !Var
  | NegInt !Var !Var
  | None !Var
  | NotBool !Var !Var
  | NotInt !Var !Var
  | OpenIn !Var
  | OpenOut !Var
  | Option
    !Var           -- ^ Some activation.
    !Var           -- ^ Condition.
    !(Vector Var)  -- ^ Extra inputs.
    !(Vector Var)  -- ^ Extra outputs.
  | OptionElse
    !Var           -- ^ None activation.
    !Var           -- ^ Some activation.
    !Var           -- ^ Condition.
    !(Vector Var)  -- ^ Extra inputs.
    !(Vector Var)  -- ^ Extra outputs.
  | OrBool !Var !Var !Var
  | OrInt !Var !Var !Var
  | Pair !Var !Var !Var
  | Print !Var !Var
  | Rest !Var !Var
  | Set !Var !Var !Var !Var
  | ShowFloat !Var !Var
  | ShowInt !Var !Var
  | Some !Var !Var
  | Stderr !Var
  | Stdin !Var
  | Stdout !Var
  | SubFloat !Var !Var !Var
  | SubInt !Var !Var !Var
  | Tail !Var !Var
  | UnsafePurify11 !Var !Var
  | XorBool !Var !Var !Var
  | XorInt !Var !Var !Var

instance Show BuiltinCall where
  show = Text.unpack . toText

instance ToText BuiltinCall where
  toText builtin = case builtin of
    AddFloat a b out     -> bind out ["addFloat", toText a, toText b]
    AddInt a b out       -> bind out ["addInt", toText a, toText b]
    AddVector a b out    -> bind out ["addVector", toText a, toText b]
    AndBool a b out      -> bind out ["andBool", toText a, toText b]
    AndInt a b out       -> bind out ["andInt", toText a, toText b]
    CharToInt a out      -> bind out ["charToInt", toText a]
    Close a              -> bindNone ["close", toText a]
    DivFloat a b out     -> bind out ["divFloat", toText a, toText b]
    DivInt a b out       -> bind out ["divInt", toText a, toText b]
    EqFloat a b out      -> bind out ["eqFloat", toText a, toText b]
    EqInt a b out        -> bind out ["eqInt", toText a, toText b]
    Exit a               -> bindNone ["exit", toText a]
    First a out          -> bind out ["first", toText a]
    FromLeft a out       -> bind out ["fromLeft", toText a]
    FromRight a out      -> bind out ["fromRight", toText a]
    FromSome a out       -> bind out ["fromSome", toText a]
    GeFloat a b out      -> bind out ["geFloat", toText a, toText b]
    GeInt a b out        -> bind out ["geInt", toText a, toText b]
    Get a b out          -> bind out ["get", toText a, toText b]
    GetLine a out        -> bind out ["getLine", toText a]
    GtFloat a b out      -> bind out ["gtFloat", toText a, toText b]
    GtInt a b out        -> bind out ["gtInt", toText a, toText b]
    Init a out           -> bind out ["init", toText a]
    IntToChar a out      -> bind out ["intToChar", toText a]
    LeFloat a b out      -> bind out ["leFloat", toText a, toText b]
    LeInt a b out        -> bind out ["leInt", toText a, toText b]
    MakeLeft a out       -> bind out ["makeLeft", toText a]
    Length a out         -> bind out ["length", toText a]
    LtFloat a b out      -> bind out ["ltFloat", toText a, toText b]
    LtInt a b out        -> bind out ["ltInt", toText a, toText b]
    ModFloat a b out     -> bind out ["modFloat", toText a, toText b]
    ModInt a b out       -> bind out ["modInt", toText a, toText b]
    MulFloat a b out     -> bind out ["mulFloat", toText a, toText b]
    MulInt a b out       -> bind out ["mulInt", toText a, toText b]
    NeFloat a b out      -> bind out ["neFloat", toText a, toText b]
    NeInt a b out        -> bind out ["neInt", toText a, toText b]
    NegFloat a out       -> bind out ["negFloat", toText a]
    NegInt a out         -> bind out ["negInt", toText a]
    None out             -> bind out ["none"]
    NotBool a out        -> bind out ["notBool", toText a]
    NotInt a out         -> bind out ["notInt", toText a]
    OpenIn out           -> bind out ["openIn"]
    OpenOut out          -> bind out ["openOut"]
    OrBool a b out       -> bind out ["orBool", toText a, toText b]
    OrInt a b out        -> bind out ["orInt", toText a, toText b]
    Pair a b out         -> bind out ["pair", toText a, toText b]
    Print a b            -> bindNone ["print", toText a, toText b]
    Rest a out           -> bind out ["rest", toText a]
    MakeRight a out      -> bind out ["makeRight", toText a]
    Set a b c out        -> bind out ["set", toText a, toText b, toText c]
    ShowFloat a out      -> bind out ["showFloat", toText a]
    ShowInt a out        -> bind out ["showInt", toText a]
    Some a out           -> bind out ["some", toText a]
    Stderr out           -> bind out ["stderr"]
    Stdin out            -> bind out ["stdin"]
    Stdout out           -> bind out ["stdout"]
    SubFloat a b out     -> bind out ["subFloat", toText a, toText b]
    SubInt a b out       -> bind out ["subInt", toText a, toText b]
    Tail a out           -> bind out ["tail", toText a]
    UnsafePurify11 a out -> bind out ["unsafePurify11", toText a]
    XorBool a b out      -> bind out ["xorBool", toText a, toText b]
    XorInt a b out       -> bind out ["xorInt", toText a, toText b]
    Apply func inputs outputs -> bindMany outputs
        ["apply", toText func, unwordsVector inputs]
    Choice leftFunc cond inputs outputs -> bindMany outputs
        [ "choice", toText cond
        , toText leftFunc
        , unwordsVector inputs
        ]
    ChoiceElse rightFunc leftFunc cond inputs outputs -> bindMany outputs
        [ "choiceElse", toText cond
        , toText leftFunc, toText rightFunc
        , unwordsVector inputs
        ]
    If trueFunc cond inputs outputs -> bindMany outputs
        [ "if", toText cond
        , toText trueFunc
        , unwordsVector inputs
        ]
    IfElse falseFunc trueFunc cond inputs outputs -> bindMany outputs
        [ "ifElse", toText cond
        , toText trueFunc, toText falseFunc
        , unwordsVector inputs
        ]
    Option someFunc cond inputs outputs -> bindMany outputs
        [ "choice", toText cond
        , toText someFunc
        , unwordsVector inputs
        ]
    OptionElse noneFunc someFunc cond inputs outputs -> bindMany outputs
        [ "optionElse", toText cond
        , toText someFunc, toText noneFunc
        , unwordsVector inputs
        ]

bindMany :: Vector Var -> [Text] -> Text
bindMany vars rest = Text.unwords
  $ vectorToTextList vars ++  "<-" : rest

bind :: Var -> [Text] -> Text
bind var = bindMany (V.singleton var)

bindNone :: [Text] -> Text
bindNone = bindMany V.empty

data Var = Var !Int !VarType
  deriving (Eq)

instance Show Var where
  show = Text.unpack . toText

instance ToText Var where
  toText (Var index Closed) = "k" <> showText index
  toText (Var index Data) = "s" <> showText index
  toText (Var index Parameter) = "p" <> showText index

data VarType = Closed | Data | Parameter
  deriving (Eq)

-- TODO(strager): Clean up uses.  (Some uses (e.g. Call)
-- only use certain constructors).
newtype ClosureName = ClosureName Int

instance Show ClosureName where
  show = Text.unpack . toText

instance ToText ClosureName where
  toText (ClosureName index) = "c" <> showText index

newtype GlobalFunctionName = GlobalFunctionName Text

instance Show GlobalFunctionName where
  show = Text.unpack . toText

instance ToText GlobalFunctionName where
  toText (GlobalFunctionName name) = name

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
type FunctionWriter = WriterT (Vector Instruction) FunctionState

fragmentToSSA
  :: Fragment Typed
  -> (Function, [Definition])
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
      return Definition
        { definitionName = GlobalFunctionName (defName def)
        , definitionFunction = ssa
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
  loc <- lift $ lift get
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
  -> GlobalState Function
functionToSSA term loc = do
  (instructions, env) <- flip runStateT defaultFunctionEnv
    . execWriterT $ do
      functionPrelude 0  -- FIXME(strager)
      termToSSA term
      functionEpilogue 0  -- FIXME(strager)
  return Function
    { funcInputs = envInferredInputArity env
    , funcOutputs = envInferredOutputArity env
    , funcInstructions = instructions
    , funcClosures = envClosures env
    , funcLocation = loc
    }

-- TODO(strager): Remove hacks in 'pop' and use this instead.
functionPrelude :: Int -> FunctionWriter ()
functionPrelude _arity = return ()

-- TODO(strager): Use arity instead of 'pop'-like hacks.
functionEpilogue :: Int -> FunctionWriter ()
functionEpilogue _arity = do
  env <- lift get
  let returnValues = envDataStack env
  lift $ put env
    { envDataStack = []
    , envInferredOutputArity = length returnValues
    }
  tell . V.singleton $ Return
    (V.reverse $ V.fromList returnValues)
    UnknownLocation

setLocation :: Location -> FunctionWriter ()
setLocation = lift . lift . put

termToSSA :: Typed -> FunctionWriter ()
termToSSA theTerm = setLocation UnknownLocation{- FIXME -} >> case theTerm of
  Typed.Builtin builtin loc type_ -> builtinToSSA builtin loc type_
  Typed.Call name loc _type -> do
    -- HACK(strager): _type has supliferous scalars on
    -- either side of the function type, throwing off
    -- functionArity.  We therefore look up the type from
    -- the function's definition.
    fn <- lift . lift $ lookUpFunction name
    let type_ = Typed.typedType (Type.unScheme (defTerm fn))

    case Type.functionArity type_ of
      Nothing -> funcFailure
        $ "Could not get arity of function " ++ show fn
      Just (inArity, outArity) -> do
        inputs <- lift $ V.replicateM inArity pop
        outputs <- lift $ V.replicateM outArity push
        let funcName = GlobalFunctionName (defName fn)
        tell . V.singleton $ Call funcName (V.reverse inputs) outputs loc
  Typed.Compose terms _loc _type -> V.mapM_ termToSSA terms
  Typed.From{} -> return ()
  Typed.PairTerm a b loc _type -> do
    aVar <- termToSSA a >> lift pop
    bVar <- termToSSA b >> lift pop
    pairVar <- lift push
    tell . V.singleton $ PairTerm aVar bVar pairVar loc
  Typed.Push value loc _type -> valueToSSA value loc
  Typed.Scoped term _loc _type -> do
    var <- lift pop
    lift $ pushLocalVar var
    termToSSA term
    poppedVar <- lift popLocal
    unless (var == poppedVar)
      . lift . lift . failure
      $ "Local mismatch (pushed " ++ show var ++ " vs popped " ++ show poppedVar ++ ")"
  Typed.To{} -> return ()
  Typed.VectorTerm values loc _type -> do
    vars <- V.mapM (\v -> termToSSA v >> lift pop) values
    var <- lift push
    tell . V.singleton $ Vector vars var loc

builtinToSSA
  :: Builtin
  -> Location
  -> Type Type.Scalar
  -> FunctionWriter ()
builtinToSSA theBuiltin loc type_ = do
  mbuiltin <- lift . maybe (return Nothing) (liftM Just) $ case theBuiltin of
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
    Builtin.Apply -> Just $ Apply
      <$> pop
      <*> V.replicateM inputs pop
      <*> V.replicateM outputs push
    Builtin.Choice -> Just $ Choice
      <$> pop
      <*> pop
      <*> V.replicateM (inputs - 1) pop
      <*> V.replicateM outputs push
    Builtin.ChoiceElse -> Just $ ChoiceElse
      <$> pop
      <*> pop
      <*> pop
      <*> V.replicateM (inputs - 1) pop
      <*> V.replicateM outputs push
    Builtin.If -> Just $ If
      <$> pop
      <*> pop
      <*> V.replicateM inputs pop
      <*> V.replicateM outputs push
    Builtin.IfElse -> Just $ IfElse
      <$> pop
      <*> pop
      <*> pop
      <*> V.replicateM inputs pop
      <*> V.replicateM outputs push
    Builtin.Option -> Just $ Option
      <$> pop
      <*> pop
      <*> V.replicateM (inputs - 1) pop
      <*> V.replicateM outputs push
    Builtin.OptionElse -> Just $ OptionElse
      <$> pop
      <*> pop
      <*> pop
      <*> V.replicateM inputs pop
      <*> V.replicateM outputs push
  case mbuiltin of
    Nothing -> return ()
    Just builtin -> tell . V.singleton $ CallBuiltin builtin loc
  where
  inputs, outputs :: Int
  ~(~inputs, ~outputs) = case type_ of
    Type.Function (_ Type.:. functionType) _rhs _effect _loc
      -> fromMaybe (err $ "failed to get arity of type " ++ show functionType)
        $ Type.functionArity functionType
    _ -> err $ "failed to find function argument for builtin " ++ show theBuiltin ++ " in type " ++ show type_
  err :: String -> a
  err m = error $ "Kitten.SSA.builtinToSSA: " ++ m

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
  Typed.Closed name -> lift . pushVar
    $ Var (nameIndex name) Closed
  Typed.Closure closedNames term -> do
    closedVars <- V.forM closedNames $ \closed -> case closed of
      ClosedName name -> lift $ getLocal name
      ReclosedName name -> return $ Var (nameIndex name) Closed
    func <- lift . lift $ functionToSSA term loc
    let
      closure = Closure
        { closureClosed = V.length closedVars
        , closureFunction = func
        }
    name <- lift $ addClosure closure
    var <- lift push
    tell . V.singleton $ Activation name closedVars var loc
  Typed.Float value -> one $ Float value
  Typed.Int value -> one $ Int value
  Typed.Local name -> do
    local <- lift $ getLocal name
    lift $ pushVar local
  Typed.String value -> do
    charVars <- mapM (\c -> one (Char c) >> lift pop)
      $ Text.unpack value
    one $ Vector (V.fromList charVars)

  Typed.Unit -> removeFromASTPlease "Unit"
  where
  one
    :: (Var -> Location -> Instruction)
    -> FunctionWriter ()
  one f = do
    var <- lift push
    tell . V.singleton $ f var loc

  -- | FIXME(strager): Several Typed.Value AST
  -- constuctors should be removed, as they are only need
  -- for the interpreter.
  removeFromASTPlease :: String -> FunctionWriter ()
  removeFromASTPlease ctorName = funcFailure
    $ "Kitten.SSA.valueToSSA: remove Typed.Value(" ++ ctorName ++ ")"
