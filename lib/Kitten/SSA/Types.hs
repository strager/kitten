{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE StandaloneDeriving #-}

module Kitten.SSA.Types
  ( ADefinition(..)
  , AFunction(..)
  , BuiltinCall(..)
  , Closure(..)
  , ClosureName(..)
  , Definition(..)
  , Form(..)
  , Function(..)
  , GlobalFunctionName(..)
  , Instruction(..)
  , RowArity(..)
  , RowVar(..)
  , TemplateParameter(..)
  , TemplateParameters(..)
  , TemplateVar(..)
  , Var(..)
  , VarType(..)

  , Upcast(..)
  , Downcast(..)

  , adefinitionName
  , afunctionToText
  , functionToText
  , adefinitionFunction
  ) where

import Control.Applicative ((<$>))
import Data.Monoid ((<>))
import Data.Text (Text)
import Data.Vector (Vector)
import Unsafe.Coerce (unsafeCoerce)

import qualified Data.Vector as V

import Kitten.Location (Location)
import Kitten.Util.Text (ToText(..), showText)

import qualified Kitten.Kind as Type
import qualified Kitten.Type as Type
import qualified Kitten.Util.Text as Text

{-
data FunctionRef
  = NormalRef !GlobalFunctionName
  | TemplateRef !GlobalFunctionName !(Vector TemplateArgument)

data TemplateArgument
  = RowArg !Int
-}

-- | A kind used to distinguish SSA forms which are
-- templated (i.e. functions returning SSA forms) versus
-- forms which are normalized.
--
-- There is an (unenforced) subtype relation 'c Normal' <:
-- 'c Template' given a GADT 'c'.
data Form = Template | Normal

data AFunction
  = NormalFunction !(Function Normal)
  | TemplateFunction !(Function Template)

instance Show AFunction where
  show = Text.unpack . toText

instance ToText AFunction where
  toText (NormalFunction f) = toText f
  toText (TemplateFunction f) = toText f

data Function (form :: Form) where
  Function ::
    { funcInputs :: !(RowArity form)
    , funcOutputs :: !(RowArity form)
    , funcInstructions :: !(Vector (Instruction form))

    , funcClosures :: !(Vector Closure)
    -- ^ Closures available only to this function.

    , funcTemplateParameters :: TemplateParameters form
    -- ^ Template parameters, if this function is a
    -- template.

    , funcLocation :: !Location
    } -> Function form

instance Show (Function form) where
  show = Text.unpack . toText

instance ToText (Function form) where
  toText = functionToText "<unknown>"

functionToText :: Text -> Function form -> Text
functionToText name Function{..} = Text.unlines
  [ Text.concat

    [ name
    , case funcTemplateParameters of
      NoParameters -> ""
      Parameters params
        -> " <"
        <> Text.intercalate " "
          (V.toList $ V.imap showTemplateParameter params)
        <> ">"
    , " ("
    , toText funcInputs
    , " -> "
    , toText funcOutputs
    , "):"
    ]
  , Text.indent $ vectorToLines funcInstructions
  , Text.indent . Text.unlines . V.toList $ V.imap
    (\index closure -> afunctionToText
      (toText (ClosureName index)) (closureFunction closure))
    funcClosures
  ]
  where
  showTemplateParameter :: Int -> TemplateParameter -> Text
  showTemplateParameter i param
    = toText (TemplateVar i) <> ":" <> toText param

data ADefinition
  = NormalDefinition !(Definition Normal)
  | TemplateDefinition !(Definition Template)

data Definition (form :: Form) = Definition
  { definitionName :: !GlobalFunctionName
  , definitionFunction :: !(Function form)
  }

adefinitionName :: ADefinition -> GlobalFunctionName
adefinitionName (NormalDefinition def) = definitionName def
adefinitionName (TemplateDefinition def) = definitionName def

adefinitionFunction :: ADefinition -> AFunction
adefinitionFunction (NormalDefinition def) = NormalFunction $ definitionFunction def
adefinitionFunction (TemplateDefinition def) = TemplateFunction $ definitionFunction def

data Closure = Closure
  { closureClosed :: !Int
  -- ^ Number of variables capture by this closure.
  , closureFunction :: !AFunction
  }

-- * Instructions.

-- | An SSA instruction.  Arguments are ordered input first,
-- output last, except for 'Return'.
data Instruction (form :: Form) where
  Activation
    :: !ClosureName
    -> !(Vector (Var form))     -- ^ Captured variables.
    -> !(Var Normal)
    -> !Location
    -> Instruction form

  Bool
    :: !Bool
    -> !(Var Normal)
    -> !Location
    -> Instruction form

  Char
    :: !Char
    -> !(Var Normal)
    -> !Location
    -> Instruction form

  Call
    :: !GlobalFunctionName
    -> !(RowVar form)
    -> !(RowVar form)
    -> !Location
    -> Instruction form

  CallBuiltin
    :: !(BuiltinCall form)
    -> !Location
    -> Instruction form

  Float
    :: !Double
    -> !(Var Normal)
    -> !Location
    -> Instruction form

  Int
    :: !Int
    -> !(Var Normal)
    -> !Location
    -> Instruction form

  PairTerm
    :: !(Var Normal)
    -> !(Var Normal)
    -> !(Var Normal)
    -> !Location
    -> Instruction form

  Return
    :: !(RowVar form)
    -> !Location
    -> Instruction form

  Vector
    :: !(Vector (Var Normal))
    -> !(Var Normal)
    -> !Location
    -> Instruction form

instance Show (Instruction form) where
  show = Text.unpack . toText

instance ToText (Instruction form) where
  toText instruction = case instruction of
    Activation funcName closed out _ -> Text.unwords
      [ bind out ["act", toText funcName]
      , unwordsVector closed
      ]
    Bool value out _ -> bind out
      ["bool", if value then "true" else "false"]
    Char value out _ -> bind out ["char", showText value]
    Call funcName params outs _ -> Text.unwords
      [ toText outs
      , "<-", "call", toText funcName
      , toText params
      ]
    CallBuiltin builtin _ -> toText builtin
    Float value out _ -> bind out ["float", showText value]
    Int value out _ -> bind out ["int", showText value]
    PairTerm a b out _ -> bind out ["pair", showText a, showText b]
    Return values _ -> "return " <> toText values
    Vector values out _ -> Text.unwords
      [ bind out ["vector"]
      , unwordsVector values
      ]

data BuiltinCall (form :: Form) where
  AddFloat       :: !(Var Normal) -> !(Var Normal) -> !(Var Normal) -> BuiltinCall form
  AddInt         :: !(Var Normal) -> !(Var Normal) -> !(Var Normal) -> BuiltinCall form
  AddVector      :: !(Var Normal) -> !(Var Normal) -> !(Var Normal) -> BuiltinCall form
  AndBool        :: !(Var Normal) -> !(Var Normal) -> !(Var Normal) -> BuiltinCall form
  AndInt         :: !(Var Normal) -> !(Var Normal) -> !(Var Normal) -> BuiltinCall form
  CharToInt      :: !(Var Normal) -> !(Var Normal) -> BuiltinCall form
  Close          :: !(Var Normal) -> BuiltinCall form
  DivFloat       :: !(Var Normal) -> !(Var Normal) -> !(Var Normal) -> BuiltinCall form
  DivInt         :: !(Var Normal) -> !(Var Normal) -> !(Var Normal) -> BuiltinCall form
  EqFloat        :: !(Var Normal) -> !(Var Normal) -> !(Var Normal) -> BuiltinCall form
  EqInt          :: !(Var Normal) -> !(Var Normal) -> !(Var Normal) -> BuiltinCall form
  Exit           :: !(Var Normal) -> BuiltinCall form
  First          :: !(Var Normal) -> !(Var Normal) -> BuiltinCall form
  FromLeft       :: !(Var Normal) -> !(Var Normal) -> BuiltinCall form
  FromRight      :: !(Var Normal) -> !(Var Normal) -> BuiltinCall form
  FromSome       :: !(Var Normal) -> !(Var Normal) -> BuiltinCall form
  GeFloat        :: !(Var Normal) -> !(Var Normal) -> !(Var Normal) -> BuiltinCall form
  GeInt          :: !(Var Normal) -> !(Var Normal) -> !(Var Normal) -> BuiltinCall form
  Get            :: !(Var Normal) -> !(Var Normal) -> !(Var Normal) -> BuiltinCall form
  GetLine        :: !(Var Normal) -> !(Var Normal) -> BuiltinCall form
  GtFloat        :: !(Var Normal) -> !(Var Normal) -> !(Var Normal) -> BuiltinCall form
  GtInt          :: !(Var Normal) -> !(Var Normal) -> !(Var Normal) -> BuiltinCall form
  Init           :: !(Var Normal) -> !(Var Normal) -> BuiltinCall form
  IntToChar      :: !(Var Normal) -> !(Var Normal) -> BuiltinCall form
  LeFloat        :: !(Var Normal) -> !(Var Normal) -> !(Var Normal) -> BuiltinCall form
  LeInt          :: !(Var Normal) -> !(Var Normal) -> !(Var Normal) -> BuiltinCall form
  Length         :: !(Var Normal) -> !(Var Normal) -> BuiltinCall form
  LtFloat        :: !(Var Normal) -> !(Var Normal) -> !(Var Normal) -> BuiltinCall form
  LtInt          :: !(Var Normal) -> !(Var Normal) -> !(Var Normal) -> BuiltinCall form
  MakeLeft       :: !(Var Normal) -> !(Var Normal) -> BuiltinCall form
  MakeRight      :: !(Var Normal) -> !(Var Normal) -> BuiltinCall form
  ModFloat       :: !(Var Normal) -> !(Var Normal) -> !(Var Normal) -> BuiltinCall form
  ModInt         :: !(Var Normal) -> !(Var Normal) -> !(Var Normal) -> BuiltinCall form
  MulFloat       :: !(Var Normal) -> !(Var Normal) -> !(Var Normal) -> BuiltinCall form
  MulInt         :: !(Var Normal) -> !(Var Normal) -> !(Var Normal) -> BuiltinCall form
  NeFloat        :: !(Var Normal) -> !(Var Normal) -> !(Var Normal) -> BuiltinCall form
  NeInt          :: !(Var Normal) -> !(Var Normal) -> !(Var Normal) -> BuiltinCall form
  NegFloat       :: !(Var Normal) -> !(Var Normal) -> BuiltinCall form
  NegInt         :: !(Var Normal) -> !(Var Normal) -> BuiltinCall form
  None           :: !(Var Normal) -> BuiltinCall form
  NotBool        :: !(Var Normal) -> !(Var Normal) -> BuiltinCall form
  NotInt         :: !(Var Normal) -> !(Var Normal) -> BuiltinCall form
  OpenIn         :: !(Var Normal) -> BuiltinCall form
  OpenOut        :: !(Var Normal) -> BuiltinCall form
  OrBool         :: !(Var Normal) -> !(Var Normal) -> !(Var Normal) -> BuiltinCall form
  OrInt          :: !(Var Normal) -> !(Var Normal) -> !(Var Normal) -> BuiltinCall form
  Pair           :: !(Var Normal) -> !(Var Normal) -> !(Var Normal) -> BuiltinCall form
  Print          :: !(Var Normal) -> !(Var Normal) -> BuiltinCall form
  Rest           :: !(Var Normal) -> !(Var Normal) -> BuiltinCall form
  Set            :: !(Var Normal) -> !(Var Normal) -> !(Var Normal) -> !(Var Normal) -> BuiltinCall form
  ShowFloat      :: !(Var Normal) -> !(Var Normal) -> BuiltinCall form
  ShowInt        :: !(Var Normal) -> !(Var Normal) -> BuiltinCall form
  Some           :: !(Var Normal) -> !(Var Normal) -> BuiltinCall form
  Stderr         :: !(Var Normal) -> BuiltinCall form
  Stdin          :: !(Var Normal) -> BuiltinCall form
  Stdout         :: !(Var Normal) -> BuiltinCall form
  SubFloat       :: !(Var Normal) -> !(Var Normal) -> !(Var Normal) -> BuiltinCall form
  SubInt         :: !(Var Normal) -> !(Var Normal) -> !(Var Normal) -> BuiltinCall form
  Tail           :: !(Var Normal) -> !(Var Normal) -> BuiltinCall form
  UnsafePurify11 :: !(Var Normal) -> !(Var Normal) -> BuiltinCall form
  XorBool        :: !(Var Normal) -> !(Var Normal) -> !(Var Normal) -> BuiltinCall form
  XorInt         :: !(Var Normal) -> !(Var Normal) -> !(Var Normal) -> BuiltinCall form

  Apply
    :: !(Var Normal)     -- ^ Activation.
    -> !(RowVar form)    -- ^ Inputs.
    -> !(RowVar form)    -- ^ Outputs.
    -> BuiltinCall form

  Choice
    :: !(Var Normal)     -- ^ Left activation.
    -> !(Var Normal)     -- ^ Condition.
    -> !(RowVar form)    -- ^ Extra inputs.
    -> !(RowVar form)    -- ^ Extra outputs.
    -> BuiltinCall form

  ChoiceElse
    :: !(Var Normal)     -- ^ Right activation.
    -> !(Var Normal)     -- ^ Left activation.
    -> !(Var Normal)     -- ^ Condition.
    -> !(RowVar form)    -- ^ Extra inputs.
    -> !(RowVar form)    -- ^ Extra outputs.
    -> BuiltinCall form

  If
    :: !(Var Normal)     -- ^ Truthy activation.
    -> !(Var Normal)     -- ^ Condition.
    -> !(RowVar form)    -- ^ Extra inputs.
    -> !(RowVar form)    -- ^ Extra outputs.
    -> BuiltinCall form

  IfElse
    :: !(Var Normal)     -- ^ Falsy activation.
    -> !(Var Normal)     -- ^ Truthy activation.
    -> !(Var Normal)     -- ^ Condition.
    -> !(RowVar form)    -- ^ Extra inputs.
    -> !(RowVar form)    -- ^ Extra outputs.
    -> BuiltinCall form

  Option
    :: !(Var Normal)     -- ^ Some activation.
    -> !(Var Normal)     -- ^ Condition.
    -> !(RowVar form)    -- ^ Extra inputs.
    -> !(RowVar form)    -- ^ Extra outputs.
    -> BuiltinCall form

  OptionElse
    :: !(Var Normal)     -- ^ None activation.
    -> !(Var Normal)     -- ^ Some activation.
    -> !(Var Normal)     -- ^ Condition.
    -> !(RowVar form)    -- ^ Extra inputs.
    -> !(RowVar form)    -- ^ Extra outputs.
    -> BuiltinCall form

instance Show (BuiltinCall form) where
  show = Text.unpack . toText

instance ToText (BuiltinCall form) where
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
    Apply func inputs outputs -> bindRow outputs
        ["apply", toText func, toText inputs]
    Choice leftFunc cond inputs outputs -> bindRow outputs
        [ "choice", toText cond
        , toText leftFunc
        , toText inputs
        ]
    ChoiceElse rightFunc leftFunc cond inputs outputs -> bindRow outputs
        [ "choiceElse", toText cond
        , toText leftFunc, toText rightFunc
        , toText inputs
        ]
    If trueFunc cond inputs outputs -> bindRow outputs
        [ "if", toText cond
        , toText trueFunc
        , toText inputs
        ]
    IfElse falseFunc trueFunc cond inputs outputs -> bindRow outputs
        [ "ifElse", toText cond
        , toText trueFunc, toText falseFunc
        , toText inputs
        ]
    Option someFunc cond inputs outputs -> bindRow outputs
        [ "choice", toText cond
        , toText someFunc
        , toText inputs
        ]
    OptionElse noneFunc someFunc cond inputs outputs -> bindRow outputs
        [ "optionElse", toText cond
        , toText someFunc, toText noneFunc
        , toText inputs
        ]

-- TODO(strager): Make variable index a newtype.
data Var (form :: Form) where
  Var :: !Int -> !(VarType form) -> Var form

deriving instance Eq (Var form)

instance Show (Var form) where
  show = Text.unpack . toText

instance ToText (Var form) where
  toText (Var index Closed) = "k" <> showText index
  toText (Var index Data) = "s" <> showText index
  toText (Var index Parameter) = "p" <> showText index
  toText (Var index (RowVar t))
    = "<" <> toText t <> ">r" <> showText index

data VarType (form :: Form) where
  Closed :: VarType form
  Data :: VarType form
  Parameter :: VarType form
  RowVar :: !TemplateVar -> VarType Template

deriving instance Eq (VarType form)

-- * Rows.

data RowArity (form :: Form) where
  ScalarArity :: !Int -> RowArity form
  TemplateArity
    :: !TemplateVar  -- ^ Template row variable.
    -> !Int          -- ^ Extra scalars (like 'ScalarArity').
    -> RowArity Template

instance Show (RowArity form) where
  show = Text.unpack . toText

instance ToText (RowArity form) where
  toText (ScalarArity arity) = showText arity
  toText (TemplateArity var scalars)
    = "<" <> toText var <> ">+" <> showText scalars

data RowVar (form :: Form) where
  ScalarVars :: !(Vector (Var form)) -> RowVar form
  TemplateRowScalarVars
    :: !TemplateVar              -- ^ Template row variable.
    -> !(Var form)               -- ^ N-ple of row values.
    -> !(Vector (Var Template))  -- ^ Extra scalars (like 'ScalarVars').
    -> RowVar Template

instance Show (RowVar form) where
  show = Text.unpack . toText

instance ToText (RowVar form) where
  toText (ScalarVars scalars) = unwordsVector scalars
  toText (TemplateRowScalarVars templateVar var scalars)
    = toText templateVar <> "("
    <> Text.unwords (toText var : map toText (V.toList scalars))
    <> ")"

-- * Names.

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

-- * Templates.

data TemplateParameter
  = RowParam !(Type.TypeName Type.Row)
  deriving (Eq, Ord)

instance Show TemplateParameter where
  show = Text.unpack . toText

instance ToText TemplateParameter where
  toText (RowParam var) = "row(" <> toText var <> ")"

newtype TemplateVar = TemplateVar Int
  deriving (Eq)

instance Show TemplateVar where
  show = Text.unpack . toText

instance ToText TemplateVar where
  toText (TemplateVar var) = "<t" <> showText var <> ">"

data TemplateParameters (form :: Form) where
  NoParameters :: TemplateParameters Normal
  Parameters
    -- TODO(strager): Disallow empty vector!
    :: !(Vector TemplateParameter)
    -> TemplateParameters Template

-- * Utilities.

mapVector :: (a -> b) -> Vector a -> [b]
mapVector f = map f . V.toList

vectorToTextList :: (ToText a) => Vector a -> [Text]
vectorToTextList = mapVector toText

unwordsVector :: (ToText a) => Vector a -> Text
unwordsVector = Text.unwords . map toText . V.toList

vectorToLines :: (ToText a) => Vector a -> Text
vectorToLines = Text.unlines . vectorToTextList

afunctionToText :: Text -> AFunction -> Text
afunctionToText name = \case
  NormalFunction f -> functionToText name f
  TemplateFunction f -> functionToText name f

bindRow :: RowVar form -> [Text] -> Text
bindRow row rest = toText row <> " <- " <> Text.unwords rest

bind :: Var form -> [Text] -> Text
bind var = bindRow (ScalarVars (V.singleton var))

bindNone :: [Text] -> Text
bindNone = bindRow (ScalarVars V.empty)

class Upcast (c :: Form -> *) (from :: Form) (to :: Form) where
  upcast :: c from -> c to
  upcast = unsafeCoerce

instance Upcast Var form Template
instance Upcast VarType form Template

class Downcast (c :: Form -> *) (from :: Form) (to :: Form) where
  downcast :: c from -> Maybe (c to)

instance Downcast Instruction Template Normal where
  -- FIXME(strager): Should throw a runtime error upon failure.
  -- FIXME(strager): !!!
  -- THIS IS SUPER UNSAFE!
  -- THIS IS SUPER UNSAFE!
  -- THIS IS SUPER UNSAFE!
  --
  -- XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX
  -- XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX
  -- XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX
  -- XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX
  -- XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX
  -- XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX
  downcast = Just . unsafeCoerce

instance Downcast RowArity Template Normal where
  downcast = \case
    ScalarArity arity -> Just $ ScalarArity arity
    TemplateArity{} -> Nothing

instance Downcast Function Template Normal where
  downcast Function{..} = do
    inputs <- downcast funcInputs
    outputs <- downcast funcOutputs
    instructions <- V.mapM downcast funcInstructions
    -- NOTE(strager): GHC does not support type-changing
    -- GADT record updates.
    return Function
      { funcInputs = inputs
      , funcOutputs = outputs
      , funcInstructions = instructions
      , funcClosures = funcClosures
      , funcTemplateParameters = NoParameters
      , funcLocation = funcLocation
      }

instance Downcast Var Template Normal where
  downcast (Var index varType)
    = Var index <$> downcast varType

instance Downcast VarType Template Normal where
  downcast = \case
    Closed -> Just Closed
    Data -> Just Data
    Parameter -> Just Parameter
    RowVar{} -> Nothing
