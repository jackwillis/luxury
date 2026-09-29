module Main where

import Control.Monad (foldM_)
import System.Environment (getArgs)
import System.Exit (exitFailure)
import System.IO (hPutStrLn, stderr)

import Env (Env)
import qualified Env
import Eval (eval, renderEvalError)
import Reader (readProgram, renderReaderError)
import SExpr (SExpr)
import qualified Value


main :: IO ()
main = do
  arguments <- getArgs

  case arguments of
    [] ->
      getContents >>= runProgram

    ["-h"] ->
      help

    ["--help"] ->
      help

    ["-v"] ->
      version

    ["--version"] ->
      version

    [path] ->
      readFile path >>= runProgram

    _ ->
      usageError


runProgram :: String -> IO ()
runProgram source =
  case readProgram source of
    Left readerError -> do
      hPutStrLn stderr (renderReaderError readerError)
      exitFailure

    Right expressions ->
      evaluateProgram Env.initial expressions

evaluateProgram :: Env -> [SExpr] -> IO ()
evaluateProgram env expressions =
  foldM_ evaluateExpression env expressions

evaluateExpression :: Env -> SExpr -> IO Env
evaluateExpression env expression =
  case eval env expression of
    Left evalError -> do
      hPutStrLn stderr (renderEvalError evalError)
      exitFailure

    Right value -> do
      putStrLn (Value.render value)
      pure env


help :: IO ()
help =
  putStrLn $
    unlines
      [ "Usage: luxury [OPTION] [FILE]"
      , ""
      , "Run a Luxury Scheme program."
      , ""
      , "With no FILE, read the program from standard input."
      , ""
      , "Options:"
      , "  -h, --help       Show this help"
      , "  -v, --version    Show version information"
      ]

version :: IO ()
version =
  putStrLn "Luxury Scheme 0.1.0.0"

usageError :: IO ()
usageError = do
  hPutStrLn stderr "Usage: luxury [OPTION] [FILE]"
  hPutStrLn stderr "Try 'luxury --help' for more information."
  exitFailure
