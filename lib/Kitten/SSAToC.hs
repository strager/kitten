{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

module Kitten.SSAToC
  ( main
  , prelude
  , ssaDefinitionToC
  , ssaDefinitionToCProto
  , ssaFunctionToC
  , ssaFunctionToCProto
  ) where

import Data.Monoid
import Data.Text (Text)
import Data.Vector (Vector)

import qualified Data.Char as Char
import qualified Data.Vector as V

import Kitten.Builtin (Builtin)
import Kitten.Location (Location)
import Kitten.Util.List (unfoldToN)
import Kitten.Util.Text (ToText(toText), showText)

import qualified Kitten.Builtin as Builtin
import qualified Kitten.SSA as SSA
import qualified Kitten.Util.Text as Text

class Mangle a where
  mangle :: a -> Text

data Name
  = FunctionName !SSA.GlobalFunctionName ![SSA.ClosureName]
  | BuiltinName !Builtin
  | RuntimeName !RuntimeFunction

data ActivationVar = ActivationVar

instance Mangle ActivationVar where
  mangle ActivationVar = "act"

data TypeName
  = Value
  | Tuple !Int

instance Mangle TypeName where
  mangle typeName = case typeName of
    Value -> "ktn_value"
    Tuple count -> "ktn_tuple" <> showText count

instance Mangle (Maybe TypeName) where
  mangle = maybe "void" mangle

typed :: TypeName -> SSA.Var -> Text
typed typeName var = mangle typeName <> " " <> mangle var

data RuntimeFunction = Raw !Text  -- TODO(strager)

rt :: Text -> Name
rt = RuntimeName . Raw

main :: SSA.GlobalFunctionName -> Text
main name
  = "int main(int argc, char **argv) {\n\
    \  (void) " <> mangle (FunctionName name []) <> "();\n\
    \  return 0;\n\
    \}\n"

prelude :: Text
prelude
  = "/* Automatically generated from the Kitten compiler. */\n\
    \#include <kitten_runtime.h>\n\
    \\n"

sanitizeIdentifier :: Text -> Text
sanitizeIdentifier = Text.concatMap f
  where
  f :: Char -> Text
  f c
    | Char.isAscii c && Char.isAlphaNum c = Text.singleton c
    | otherwise = "_" <> showText (Char.ord c) <> "_"

instance Mangle Name where
  mangle name = case name of
    FunctionName globalName closures
      -> "ktnapp_" <> sanitizeIdentifier (toText globalName)
      <> Text.concat (map (("_" <>) . sanitizeIdentifier . toText) closures)
    BuiltinName builtin -> "ktn_builtin_" <> case builtin of
      Builtin.AddFloat       -> "addFloat"
      Builtin.AddInt         -> "addInt"
      Builtin.AddVector      -> "addVector"
      Builtin.AndBool        -> "andBool"
      Builtin.AndInt         -> "andInt"
      Builtin.Apply          -> "apply"
      Builtin.CharToInt      -> "charToInt"
      Builtin.Choice         -> "choice"
      Builtin.ChoiceElse     -> "choiceElse"
      Builtin.Close          -> "close"
      Builtin.DivFloat       -> "divFloat"
      Builtin.DivInt         -> "divInt"
      Builtin.EqFloat        -> "eqFloat"
      Builtin.EqInt          -> "eqInt"
      Builtin.Exit           -> "exit"
      Builtin.First          -> "first"
      Builtin.FromLeft       -> "fromLeft"
      Builtin.FromRight      -> "fromRight"
      Builtin.FromSome       -> "fromSome"
      Builtin.GeFloat        -> "geFloat"
      Builtin.GeInt          -> "geInt"
      Builtin.Get            -> "get"
      Builtin.GetLine        -> "getLine"
      Builtin.GtFloat        -> "gtFloat"
      Builtin.GtInt          -> "gtInt"
      Builtin.If             -> "if"
      Builtin.IfElse         -> "ifElse"
      Builtin.Impure         -> error "Kitten.SSAToC.toText: cannot mangle 'Impure'"
      Builtin.Init           -> "init"
      Builtin.IntToChar      -> "intToChar"
      Builtin.LeFloat        -> "leFloat"
      Builtin.LeInt          -> "leInt"
      Builtin.Left           -> "left"
      Builtin.Length         -> "length"
      Builtin.LtFloat        -> "ltFloat"
      Builtin.LtInt          -> "ltInt"
      Builtin.ModFloat       -> "modFloat"
      Builtin.ModInt         -> "modInt"
      Builtin.MulFloat       -> "mulFloat"
      Builtin.MulInt         -> "mulInt"
      Builtin.NeFloat        -> "neFloat"
      Builtin.NeInt          -> "neInt"
      Builtin.NegFloat       -> "negFloat"
      Builtin.NegInt         -> "negInt"
      Builtin.None           -> "none"
      Builtin.NotBool        -> "notBool"
      Builtin.NotInt         -> "notInt"
      Builtin.OpenIn         -> "openIn"
      Builtin.OpenOut        -> "openOut"
      Builtin.Option         -> "option"
      Builtin.OptionElse     -> "optionElse"
      Builtin.OrBool         -> "orBool"
      Builtin.OrInt          -> "orInt"
      Builtin.Pair           -> "pair"
      Builtin.Print          -> "print"
      Builtin.Rest           -> "rest"
      Builtin.Right          -> "right"
      Builtin.Set            -> "set"
      Builtin.ShowFloat      -> "showFloat"
      Builtin.ShowInt        -> "showInt"
      Builtin.Some           -> "some"
      Builtin.Stderr         -> "stderr"
      Builtin.Stdin          -> "stdin"
      Builtin.Stdout         -> "stdout"
      Builtin.SubFloat       -> "subFloat"
      Builtin.SubInt         -> "subInt"
      Builtin.Tail           -> "tail"
      Builtin.UnsafePurify11 -> "unsafePurify11"
      Builtin.XorBool        -> "xorBool"
      Builtin.XorInt         -> "xorInt"
    RuntimeName runtimeFunction -> case runtimeFunction of
      Raw x -> "ktn_" <> x

instance Mangle SSA.Var where
  mangle = sanitizeIdentifier . toText

typeOfArity :: Int -> Maybe TypeName
typeOfArity arity
  | arity < 0 = error "Kitten.SSAToC.typeOfArity: negative arity"
  | arity == 0 = Nothing
  | arity == 1 = Just Value
  | otherwise = Just (Tuple arity)

comment :: Text -> Text
comment message = "/* " <> sanitize message <> " */"
  where
  sanitize :: Text -> Text
  sanitize = Text.replace "*/" "* /"

locationComment :: Location -> Text
locationComment = comment . toText

ssaDefinitionToC :: SSA.Definition -> Text
ssaDefinitionToC def = ssaFunctionToC
  (SSA.definitionFunction def)
  (SSA.definitionName def)

ssaDefinitionToCProto :: SSA.Definition -> Text
ssaDefinitionToCProto def = ssaFunctionToCProto
  (SSA.definitionFunction def)
  (SSA.definitionName def)
  []

ssaFunctionToC
  :: SSA.Function
  -> SSA.GlobalFunctionName
  -> Text
ssaFunctionToC func name = ssaFunctionToC' func name [] ""

ssaFunctionToC'
  :: SSA.Function
  -> SSA.GlobalFunctionName
  -> [SSA.ClosureName]
  -> Text -- ^ Prelude.
  -> Text
ssaFunctionToC' func globalName closureNames funcPrelude
  = Text.intercalate "\n" (this : closures)
  where
  this :: Text
  this = Text.concat
    [ locationComment (SSA.funcLocation func), "\n"
    , functionSignatureToC func globalName closureNames
    , " {\n"
    , Text.indent (funcPrelude <> body)
    , "}\n"
    ]
  closures :: [Text]
  closures = V.toList $ V.imap
    (\index closure -> ssaFunctionToC'
      (SSA.closureFunction closure)
      globalName
      (SSA.ClosureName index : closureNames)
      (closurePrelude closure))
    (SSA.funcClosures func)
  body :: Text
  body = Text.unlines . map
    (ssaInstructionToC globalName closureNames)
    $ V.toList (SSA.funcInstructions func)

valueAt :: Text -> Int -> Text
valueAt struct index
  = "(" <> struct <> ").values[" <> showText index <> "]"

closurePrelude :: SSA.Closure -> Text
closurePrelude closure = Text.unlines
  . unfoldToN (SSA.closureClosed closure)
  $ \index -> declareVar Value (SSA.Var index SSA.Closed)
    . Just $ ("*" <> mangle ActivationVar) `valueAt` index

commaSeparated :: [Text] -> Text
commaSeparated = Text.intercalate ", "

signature
  :: Maybe TypeName  -- ^ Return type.
  -> Text            -- ^ Function name.
  -> [Text]          -- ^ Parameters types and names.
  -> Text
signature returnType name parameters
  = mangle returnType <> " " <> name
  <> "(" <> commaSeparated parameters <> ")"

closureReturnTypeFromArity
  :: Int  -- ^ Input parameter count.
  -> Text
closureReturnTypeFromArity outputs
  = mangle (typeOfArity outputs)

closureParameterTypesFromArity
  :: Int  -- ^ Output parameter count.
  -> Text
closureParameterTypesFromArity inputs
  = "(" <> commaSeparated parameters <> ")"
  where
  parameters :: [Text]
  parameters
    = "ktn_activation *"
    : map mangle (replicate inputs Value)

functionSignatureToC
  :: SSA.Function
  -> SSA.GlobalFunctionName
  -> [SSA.ClosureName]
  -> Text
functionSignatureToC SSA.Function{..} globalName closureNames
  = signature (typeOfArity funcOutputs)
    (mangle $ FunctionName globalName closureNames)
    $ case closureNames of
      [] -> formalParameters
      (_:_) -> ("ktn_activation *" <> mangle ActivationVar)
        : formalParameters
  where
  formalParameters :: [Text]
  formalParameters = reverse . unfoldToN funcInputs
    $ \index -> typed Value (SSA.Var index SSA.Parameter)

ssaFunctionToCProto
  :: SSA.Function
  -> SSA.GlobalFunctionName
  -> [SSA.ClosureName]
  -> Text
ssaFunctionToCProto func globalName closureNames
  = Text.intercalate "\n" (thisProto : closureProtos)
  where
  thisProto :: Text
  thisProto = functionSignatureToC
    func globalName closureNames <> ";"
  closureProtos :: [Text]
  closureProtos = V.toList $ V.imap
    (\index closure -> ssaFunctionToCProto
      (SSA.closureFunction closure)
      globalName
      (SSA.ClosureName index : closureNames))
    (SSA.funcClosures func)

boolToC :: Bool -> Text
boolToC True = functionCall (rt "true") []
boolToC False = functionCall (rt "false") []

charToC :: Char -> Text
charToC c = functionCall (rt "char") [showText (Char.ord c)]

intToC :: Int -> Text
intToC int = functionCall (rt "int") [showText int]

floatToC :: Double -> Text
floatToC float = functionCall (rt "float") [showText float]

vectorToC :: Vector Text -> Text
vectorToC xs = functionCall (rt "vector")
  $ showText (V.length xs) : V.toList xs

declareVar
  :: TypeName    -- ^ Type.
  -> SSA.Var     -- ^ Name.
  -> Maybe Text  -- ^ Value.
  -> Text
declareVar typeName name value
  = typed typeName name
  <> maybe "" (" = " <>) value
  <> ";"

declareRawVar
  :: Text        -- ^ Type.
  -> Text        -- ^ Name.
  -> Maybe Text  -- ^ Value.
  -> Text
declareRawVar typeName name value
  = typeName <> " " <> name
  <> maybe "" (" = " <>) value
  <> ";"

functionCall
  :: Name    -- ^ Function to call.
  -> [Text]  -- ^ Arguments.
  -> Text
functionCall name arguments
  = mangle name <> "(" <> commaSeparated arguments <> ")"

macroCall
  :: Text    -- ^ Function-like macro to call.
  -> [Text]  -- ^ Arguments.
  -> Text
macroCall name arguments
  = name <> "(" <> commaSeparated arguments <> ")"

declareVars
  :: Vector SSA.Var  -- ^ Variable names.
  -> Text            -- ^ Expression returning zero or more values.
  -> Text
declareVars vars value = case V.toList vars of
  [] -> value <> ";"
  [var] -> declareVar Value var (Just value)
  varList
    -> declareRawVar tmpVarType tmpVarName (Just value) <> "\n"
    <> Text.unlines (V.toList (V.imap extractValue (V.reverse vars)))
    where
    tmpVarType :: Text
    tmpVarType = mangle (typeOfArity (V.length vars))
    tmpVarName :: Text
    tmpVarName = Text.intercalate "_"
      $ map toText varList
    extractValue :: Int -> SSA.Var -> Text
    extractValue index var
      = declareVar Value var
      $ Just (tmpVarName `valueAt` index)

ssaInstructionToC
  :: SSA.GlobalFunctionName
  -> [SSA.ClosureName]
  -> SSA.Instruction
  -> Text
ssaInstructionToC globalName closureNames instruction = case instruction of
  SSA.Activation closureName vars out loc -> mkVar out loc
    . functionCall (rt "act")
    $ function
    : showText (V.length vars)
    : map toText (V.toList vars)
    where
    function :: Text
    function = "((ktn_closure_function) (" <> functionName <> "))"
    functionName :: Text
    functionName = mangle
      $ FunctionName globalName (closureName : closureNames)
  SSA.Bool value out loc -> mkVar out loc $ boolToC value
  SSA.Char value out loc -> mkVar out loc $ charToC value
  SSA.Call name inputs outputs loc
    -> locationComment loc <> "\n"
    <> declareVars outputs
      (functionCall
        (FunctionName name [])
        (map toText (V.toList inputs)))
  SSA.CallBuiltin builtinCall loc
    -> locationComment loc <> "\n"
    <> builtinCallToC builtinCall
  SSA.Float value out loc -> mkVar out loc $ floatToC value
  SSA.Int value out loc -> mkVar out loc $ intToC value
  SSA.PairTerm a b out loc -> mkVar out loc
    $ functionCall (rt "pair") [toText a, toText b]
  SSA.Return values loc
    -> locationComment loc <> "\n"
    <> "return" <> (case V.toList values of
      [] -> ""
      [x] -> " " <> toText x
      xs
        -> " (" <> mangle (typeOfArity (V.length values)) <> ") "
        <> "{ { "
        <> commaSeparated (map toText xs)
        <> " } }")
    <> ";"
  SSA.Vector values out loc -> mkVar out loc
    $ vectorToC (V.map toText values)
  where
  mkVar :: SSA.Var -> Location -> Text -> Text
  mkVar output location value
    = locationComment location <> "\n"
    <> declareVar Value output (Just value)

builtinCallToC :: SSA.BuiltinCall -> Text
builtinCallToC builtinCall = case builtinCall of
  SSA.AddFloat a b out     -> mkVar out Builtin.AddFloat       [b, a]
  SSA.AddInt a b out       -> mkVar out Builtin.AddInt         [b, a]
  SSA.AddVector a b out    -> mkVar out Builtin.AddVector      [b, a]
  SSA.AndBool a b out      -> mkVar out Builtin.AndBool        [b, a]
  SSA.AndInt a b out       -> mkVar out Builtin.AndInt         [b, a]
  SSA.CharToInt a out      -> mkVar out Builtin.CharToInt      [a]
  SSA.Close a              -> call Builtin.Close               [a]
  SSA.DivFloat a b out     -> mkVar out Builtin.DivFloat       [b, a]
  SSA.DivInt a b out       -> mkVar out Builtin.DivInt         [b, a]
  SSA.EqFloat a b out      -> mkVar out Builtin.EqFloat        [b, a]
  SSA.EqInt a b out        -> mkVar out Builtin.EqInt          [b, a]
  SSA.Exit a               -> call Builtin.Exit                [a]
  SSA.First a out          -> mkVar out Builtin.First          [a]
  SSA.FromLeft a out       -> mkVar out Builtin.FromLeft       [a]
  SSA.FromRight a out      -> mkVar out Builtin.FromRight      [a]
  SSA.FromSome a out       -> mkVar out Builtin.FromSome       [a]
  SSA.GeFloat a b out      -> mkVar out Builtin.GeFloat        [b, a]
  SSA.GeInt a b out        -> mkVar out Builtin.GeInt          [b, a]
  SSA.Get a b out          -> mkVar out Builtin.Get            [b, a]
  SSA.GetLine a out        -> mkVar out Builtin.GetLine        [a]
  SSA.GtFloat a b out      -> mkVar out Builtin.GtFloat        [b, a]
  SSA.GtInt a b out        -> mkVar out Builtin.GtInt          [b, a]
  SSA.Init a out           -> mkVar out Builtin.Init           [a]
  SSA.IntToChar a out      -> mkVar out Builtin.IntToChar      [a]
  SSA.LeFloat a b out      -> mkVar out Builtin.LeFloat        [b, a]
  SSA.LeInt a b out        -> mkVar out Builtin.LeInt          [b, a]
  SSA.MakeLeft a out       -> mkVar out Builtin.Left           [a]
  SSA.Length a out         -> mkVar out Builtin.Length         [a]
  SSA.LtFloat a b out      -> mkVar out Builtin.LtFloat        [b, a]
  SSA.LtInt a b out        -> mkVar out Builtin.LtInt          [b, a]
  SSA.ModFloat a b out     -> mkVar out Builtin.ModFloat       [b, a]
  SSA.ModInt a b out       -> mkVar out Builtin.ModInt         [b, a]
  SSA.MulFloat a b out     -> mkVar out Builtin.MulFloat       [b, a]
  SSA.MulInt a b out       -> mkVar out Builtin.MulInt         [b, a]
  SSA.NeFloat a b out      -> mkVar out Builtin.NeFloat        [b, a]
  SSA.NeInt a b out        -> mkVar out Builtin.NeInt          [b, a]
  SSA.NegFloat a out       -> mkVar out Builtin.NegFloat       [a]
  SSA.NegInt a out         -> mkVar out Builtin.NegInt         [a]
  SSA.None out             -> mkVar out Builtin.None           []
  SSA.NotBool a out        -> mkVar out Builtin.NotBool        [a]
  SSA.NotInt a out         -> mkVar out Builtin.NotInt         [a]
  SSA.OpenIn out           -> mkVar out Builtin.OpenIn         []
  SSA.OpenOut out          -> mkVar out Builtin.OpenOut        []
  SSA.OrBool a b out       -> mkVar out Builtin.OrBool         [b, a]
  SSA.OrInt a b out        -> mkVar out Builtin.OrInt          [b, a]
  SSA.Pair a b out         -> mkVar out Builtin.Pair           [b, a]
  SSA.Print a b            -> call Builtin.Print               [b, a]
  SSA.Rest a out           -> mkVar out Builtin.Rest           [a]
  SSA.MakeRight a out      -> mkVar out Builtin.Right          [a]
  SSA.Set a b c out        -> mkVar out Builtin.Set            [c, b, a]
  SSA.ShowFloat a out      -> mkVar out Builtin.ShowFloat      [a]
  SSA.ShowInt a out        -> mkVar out Builtin.ShowInt        [a]
  SSA.Some a out           -> mkVar out Builtin.Some           [a]
  SSA.Stderr out           -> mkVar out Builtin.Stderr         []
  SSA.Stdin out            -> mkVar out Builtin.Stdin          []
  SSA.Stdout out           -> mkVar out Builtin.Stdout         []
  SSA.SubFloat a b out     -> mkVar out Builtin.SubFloat       [b, a]
  SSA.SubInt a b out       -> mkVar out Builtin.SubInt         [b, a]
  SSA.Tail a out           -> mkVar out Builtin.Tail           [a]
  SSA.UnsafePurify11 a out -> mkVar out Builtin.UnsafePurify11 [a]
  SSA.XorBool a b out      -> mkVar out Builtin.XorBool        [b, a]
  SSA.XorInt a b out       -> mkVar out Builtin.XorInt         [b, a]
  SSA.Apply func inputs outputs
    -- def __apply (.r (.r -> .s +e) -> .s +e)
    -> declareVars outputs
      . macroCall "KTN_APPLY"
      $ closureReturnTypeFromArity (V.length outputs)
      : closureParameterTypesFromArity (V.length inputs)
      : mangle func
      : map mangle (V.toList inputs)
  SSA.Choice leftFunc cond inputs outputs
    -> declareVars outputs
      . macroCall "KTN_CHOICE"
      $ closureReturnTypeFromArity (V.length outputs)
      : closureParameterTypesFromArity (V.length inputs + 1)
      : mangle cond
      : mangle leftFunc
      : map mangle (V.toList inputs)
  SSA.ChoiceElse rightFunc leftFunc cond inputs outputs
    -> declareVars outputs
      . macroCall "KTN_CHOICE_ELSE"
      $ closureReturnTypeFromArity (V.length outputs)
      : closureParameterTypesFromArity (V.length inputs + 1)
      : mangle cond
      : mangle leftFunc
      : mangle rightFunc
      : map mangle (V.toList inputs)
  SSA.If trueFunc cond inputs outputs
    -> declareVars outputs
      . macroCall "KTN_IF"
      $ closureReturnTypeFromArity (V.length outputs)
      : closureParameterTypesFromArity (V.length inputs)
      : mangle cond
      : mangle trueFunc
      : map mangle (V.toList inputs)
  SSA.IfElse falseFunc trueFunc cond inputs outputs
    -> declareVars outputs
      . macroCall "KTN_IF_ELSE"
      $ closureReturnTypeFromArity (V.length outputs)
      : closureParameterTypesFromArity (V.length inputs)
      : mangle cond
      : mangle trueFunc
      : mangle falseFunc
      : map mangle (V.toList inputs)
  SSA.Option someFunc cond inputs outputs
    -> declareVars outputs
      . macroCall "KTN_OPTION"
      $ closureReturnTypeFromArity (V.length outputs)
      : closureParameterTypesFromArity (V.length inputs + 1)
      : mangle cond
      : mangle someFunc
      : map mangle (V.toList inputs)
  SSA.OptionElse noneFunc someFunc cond inputs outputs
    -> declareVars outputs
      . macroCall "KTN_OPTION_ELSE"
      $ closureReturnTypeFromArity (V.length outputs)
      : closureParameterTypesFromArity (V.length inputs + 1)
      : closureParameterTypesFromArity (V.length inputs + 0)
      : mangle cond
      : mangle someFunc
      : mangle noneFunc
      : map mangle (V.toList inputs)
  where
  mkVar :: SSA.Var -> Builtin -> [SSA.Var] -> Text
  mkVar output builtin arguments
    = declareVar Value output . Just
    $ functionCall (BuiltinName builtin) (map toText arguments)
  call :: Builtin -> [SSA.Var] -> Text
  call builtin arguments
    = functionCall (BuiltinName builtin) (map toText arguments)
    <> ";"
