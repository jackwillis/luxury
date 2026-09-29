module Main where

import Data.Char (isSpace)
import Data.List (dropWhileEnd)
import System.Console.Haskeline (InputT, defaultSettings, getInputLine, outputStrLn, runInputT)
import Text.Printf (printf)

import Env (Env)
import qualified Env (bind, initial, lookup)
import Eval (eval, renderEvalError)
import Reader (readProgram, renderReaderError)
import SExpr (SExpr)
import qualified Value (render)

main :: IO ()
main = do
  let env = Env.bind "promptCount" 1 Env.initial
  runInputT defaultSettings (loop env)

loop :: Int -> InputT IO ()
loop env = do
  let promptCount = Env.lookup "promptCount" env
  let prompt = printf "luxury:%03d> " promptCount
  lineOrEof <- getInputLine prompt
  case lineOrEof of
    Nothing -> outputStrLn "Goodbye"
    Just userInput -> do
      case toMaybeLine userInput of
        Just line -> readEvaluatePrint env line
        Nothing   -> pure ()
      let nextEnv = Env.bind "promptCount" (promptCount + 1) env
      loop nextEnv


readEvaluatePrint :: String -> InputT IO ()
readEvaluatePrint line = do
  let readerResult = readProgram line
  case readerResult of
    Left readerError  -> outputStrLn $ renderReaderError readerError
    Right expressions -> mapM_ evaluatePrint expressions

evaluatePrint :: Env -> SExpr -> InputT IO ()
evaluatePrint env expression =
  case eval env expression of
    Left evalError -> outputStrLn $ renderEvalError evalError
    Right value    -> outputStrLn $ Value.render value


toMaybeLine :: String -> Maybe String
toMaybeLine userEntry
  | all isSpace userEntry = Nothing
  | otherwise             = Just (trimWhitespace userEntry)

trimWhitespace :: String -> String
trimWhitespace = dropWhileEnd isSpace . dropWhile isSpace
