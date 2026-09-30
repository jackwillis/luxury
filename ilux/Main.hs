module Main where

import Data.Char (isSpace)
import Data.List (dropWhileEnd)
import System.Console.Haskeline
  ( InputT
  , defaultSettings
  , getInputLine
  , historyFile
  , outputStrLn
  , runInputT
  )
import System.Directory (getHomeDirectory)
import System.FilePath ((</>))
import Text.Printf (printf)

import Env (Env)
import qualified Env
import Eval (eval, renderEvalError)
import Reader (readProgram, renderReaderError)
import SExpr (SExpr)
import qualified Value


main :: IO ()
main = do
  homeDirectory <- getHomeDirectory
  let haskelineSettings = defaultSettings
        { historyFile = Just (homeDirectory </> ".ilux_history") }
  runInputT haskelineSettings (loop Env.initial 1)


-- Main REPL loop
loop :: Env -> Int -> InputT IO ()
loop env promptCount = do
  let prompt = printf "ilux:%03d> " promptCount
  lineOrEof <- getInputLine prompt

  case lineOrEof of
    Nothing ->
      pure ()  -- EOF, exit the REPL

    Just userInput -> do
      case toMaybeLine userInput of
        Just line ->
          readEvaluatePrint env line

        Nothing ->
          pure ()  -- Ignore empty lines

      loop env (promptCount + 1)

-- Read, evaluate, and print the result of a single line of user input.
readEvaluatePrint :: Env -> String -> InputT IO ()
readEvaluatePrint env line = do
  let readerResult = readProgram line

  case readerResult of
    Left readerError ->
      outputStrLn (renderReaderError readerError)

    Right expressions ->
      mapM_ (evaluatePrint env) expressions

evaluatePrint :: Env -> SExpr -> InputT IO ()
evaluatePrint env expression =
  case eval env expression of
    Left evalError ->
      outputStrLn (renderEvalError evalError)

    Right value ->
      outputStrLn (Value.render value)


-- drop empty lines and trim whitespace
toMaybeLine :: String -> Maybe String
toMaybeLine userEntry
  | all isSpace userEntry = Nothing
  | otherwise             = Just (trimWhitespace userEntry)
  where
    trimWhitespace =
      dropWhileEnd isSpace . dropWhile isSpace
