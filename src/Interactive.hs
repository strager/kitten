{-# LANGUAGE DataKinds #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

module Interactive
  ( runInteraction
  ) where

import Control.Monad
import Control.Monad.Trans.Class
import Control.Monad.Trans.State.Strict hiding (State)
import Data.Char (isSpace)
import Data.Maybe
import Data.Monoid
import Data.Set (Set)
import Data.Text (Text)
import Prelude hiding (interact)
import System.Console.Haskeline
import Text.Parsec.Pos

import qualified Control.Exception as E
import qualified Data.Set as S
import qualified Data.Text as T
import qualified Data.Text.IO as TIO
import qualified Data.Vector as V

import Kitten.Compile (compile)
import Kitten.Error
import Kitten.Interpret
import Kitten.Location
import Kitten.Type
import Kitten.Util.Monad
import Kitten.Yarn

import qualified Kitten.Compile as Compile
import qualified Kitten.Infer.Config as Infer
import qualified Kitten.Interpret as Interpret

data Env = Env
  { envLine :: !Int
  , envProgram :: !Program
  , envStack :: [InterpreterValue]
  }

type State = StateT Env IO
type Input = InputT State

runInteraction :: IO ()
runInteraction = do
  welcome
  flip evalStateT emptyEnv $ runInputT settings interact

  where
  welcome :: IO ()
  welcome = mapM_ putStrLn
    [ "Welcome to Kitten!"
    , "Type ':help' for help or ':quit' to quit."
    ]

  emptyEnv :: Env
  emptyEnv = Env
    { envLine = 1
    , envProgram = emptyProgram
    , envStack = []
    }

settings :: Settings State
settings = defaultSettings
  { historyFile = Nothing
  , autoAddHistory = True
  }

{-

completer :: CompletionFunc State
completer = completeWord Nothing "\t \"{}[]()\\:" completePrefix

completePrefix :: String -> State [Completion]
completePrefix prefix = do
  defs <- gets replDefs
  let
    prefix' = T.pack prefix
    matching
      = V.filter (prefix' `T.isPrefixOf`) (defName <$> defs)
      <> V.filter (prefix' `T.isPrefixOf`) Builtin.names
    finished = V.length matching <= 1
    completions = map (toCompletion finished . T.unpack) (V.toList matching)
  return completions

toCompletion :: Bool -> String -> Completion
toCompletion finished name = Completion
  { replacement = name
  , display = name
  , isFinished = finished
  }

-}

interact :: Input ()
interact = do
  mLine <- getInputLine ">>> "
  case mLine of
    Nothing -> quit
    Just input -> evaluate input
  where
  evaluate line = case lookup cmd replCommandsTable of
    Just (Command {cmdAction=execute}) -> execute args
    Nothing
      -- | not (matched line) -> continue (T.pack line)
      | null line -> interact'
      | otherwise -> eval (T.pack line)
    where
    (cmd, args) =
      let (c, a) = T.break isSpace $ T.pack line
      in (c, T.strip a)

eval :: Interaction
eval input = do
  mCompiled <- do
    idGen <- lift $ gets (programIdGen . envProgram)
    stackValues <- lift $ gets envStack
    lineNumber <- lift $ gets envLine
    let
      loc = Location
        { locationStart = newPos "REPL" lineNumber 0
        , locationIndent = -1
        }
      (stackTypes, idGen') = flip runState idGen
        $ mapM (state . Interpret.typeOf loc) stackValues
    lift . modify $ \env -> env
      { envProgram = (envProgram env)
        { programIdGen = idGen' }
      }
    interactiveCompile $ \config -> config
      { Compile.source = input
      , Compile.stackTypes = V.fromList (reverse stackTypes)
      }

  whenJust mCompiled $ \ (compiled, ip, _type) -> do
    stackState <- lift $ gets envStack
    stackState' <- liftIO $ interpret (Just ip) (flattenProgram compiled) stackState
    lift . modify $ \s -> s
      { envLine = envLine s + T.count "\n" input + 1
      , envStack = stackState'
      }

  interact'

interact' :: Input ()
interact' = showStack >> interact

quit :: Input ()
quit = noop

showStack :: Input ()
showStack = do
  data_ <- lift $ gets envStack
  unless (null data_) . liftIO $ do
    putStrLn "\n----"
    mapM_ putStrLn . reverse $ map show data_

interactiveCompile
  :: (Compile.Config -> Compile.Config)
  -> Input (Maybe (Program, Int, Type Scalar))
interactiveCompile update = do
  program <- lift $ gets envProgram
  mCompiled <- do
    config <- compileConfig
    liftIO $ compile (update config) program
  case mCompiled of
    Left errors -> liftIO (printCompileErrors errors) >> return Nothing
    Right result@(program', _, _) -> do
      lift . modify $ \env -> env { envProgram = program' }
      return (Just result)

compileConfig :: Input Compile.Config
compileConfig = do
  Env{..} <- lift get
  return Compile.Config
    { Compile.dumpResolved = False
    , Compile.dumpScoped = False
    , Compile.firstLine = envLine
    , Compile.implicitPrelude = False
    , Compile.inferConfig = Infer.Config
      { enforceBottom = True
      , fragmentName = name
      }
    , Compile.libraryDirectories = []  -- TODO
    , Compile.name = name
    , Compile.predefined = mempty  -- TODO
    , Compile.source = ""
    , Compile.stackTypes = V.empty
    }
  where name = "<interactive>"

liftIO :: IO a -> Input a
liftIO = lift . lift

type LineArgs = Text
type Interaction = LineArgs -> Input ()

data Description = Description
  { descCommand :: Text
  , descHelp :: Text
  }

data Command = Command
  { cmdSymbols :: Set Text
  , cmdAction :: Interaction
  , cmdDesc :: Description
  }

newArgCommand :: [Text] -> Interaction -> Text -> Text -> Command
newArgCommand symbols func helpArgs helpText = Command
  { cmdSymbols = S.fromList fmtSymbols
  , cmdAction = func
  , cmdDesc = Description
    { descCommand = T.unwords [symbolText, helpArgs]
    , descHelp = helpText
    }
  }
  where
  fmtSymbols = map (T.cons ':') symbols
  symbolText = T.concat ["[", T.intercalate ", " fmtSymbols, "]"]

newCommand :: [Text] -> Input () -> Text -> Command
newCommand symbols func helpText =
  newArgCommand symbols (const func) "" helpText

replCommands :: [Command]
replCommands =
  [ newCommand ["c", "clear"] clear "Clear the Stack"
  , newCommand ["h", "help"] help "Display this help message"
  , newCommand ["q", "quit"] quit "Quit the Kitten REPL"
  , newArgCommand ["l", "load"] (load . T.unpack)
      "<filepath>" "Load a file into the Kitten REPL"
  , newCommand ["reset"] reset "Clear the stack and all definitions"
  , newArgCommand ["t", "type"] reportType
      "<expression>" "Print the inferred type of <expression>"
  ]

replCommandsTable :: [(Text, Command)]
replCommandsTable = toTable $ zipSymbols replCommands
  where
  zipSymbols xs = zip (map (S.toList . cmdSymbols) xs) xs
  toTable = concatMap (\(ks, v) -> map (\k -> (k, v)) ks)

clear :: Input ()
clear = do
  lift . modify $ \s -> s { envStack = [] }
  interact'

reset :: Input ()
reset = do
  idGen <- lift $ gets (programIdGen . envProgram)
  lift $ put Env
    { envLine = 1
    , envProgram = emptyProgram { programIdGen = idGen }
    , envStack = []
    }
  interact'

load :: FilePath -> Input ()
load file = do
  r <- liftIO $ (E.try (TIO.readFile file) :: IO (Either IOException Text))
  case r of
    Left e -> do
      liftIO . putStrLn $ "Error loading file:\n  " ++ show e
      interact'
    Right contents -> do
      liftIO . putStrLn $ "File loaded: " ++ file
      eval contents

help :: Input ()
help = do
  liftIO $ printColumns $ concat
    [ [ Just ("<expression>", "Evaluate <expression> and print the result")
      , Just ("def <name> (<signature>) <body>", "Introduce a definition")
      , Nothing
      ]
    , replCommandsHelp
    , [ Nothing
      , Just ("<TAB>", "Autocomplete a definition name")
      ]
    ]
  interact'

  where
  printColumns :: [Maybe (String, String)] -> IO ()
  printColumns columns = mapM_ go columns
    where
    margin = 2
    width = maximum . map (length . fst) $ catMaybes columns
    go column = case column of
      Nothing -> putStrLn ""
      Just (a, b) -> do
        putStr a
        putStr (replicate (width + margin - length a) ' ')
        putStrLn b
  replCommandsHelp =
    map (\(Command {cmdDesc=(Description fmt info)}) ->
      Just (T.unpack fmt, T.unpack info)) replCommands

reportType :: Interaction
reportType input = do
  mCompiled <- interactiveCompile $ \config -> config
    { Compile.source = input
    , Compile.inferConfig = Infer.Config
      { enforceBottom = False
      , fragmentName = Compile.name config
      }
    }
  whenJust mCompiled $ \(_compiled, _ip, type_) -> liftIO $ print type_
  interact'