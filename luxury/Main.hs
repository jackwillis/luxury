module Main where

import Data.Char (isSpace)
import Data.List (dropWhileEnd)
import System.Console.Haskeline (InputT, defaultSettings, getInputLine, outputStrLn, runInputT)
import Text.Printf (printf)

import Reader (readProgram, renderReaderError)
import SExpr

main :: IO ()
main = runInputT defaultSettings (loop 1)

loop :: Int -> InputT IO ()
loop promptCount = do
  let prompt = printf "luxury:%03d> " promptCount
  lineOrEof <- getInputLine prompt
  case lineOrEof of
    Nothing -> outputStrLn "Goodbye"
    Just userInput -> do
      case toMaybeLine userInput of
        Just line -> readEvaluatePrint line
        Nothing   -> pure ()
      loop (promptCount + 1)

readEvaluatePrint :: String -> InputT IO ()
readEvaluatePrint line = do
  let readResult = readProgram line
  case readResult of
    Left error        -> outputStrLn $ renderReaderError error 
    Right expressions -> mapM_ (outputStrLn . SExpr.render) expressions

toMaybeLine :: String -> Maybe String
toMaybeLine userEntry
  | all isSpace userEntry = Nothing
  | otherwise             = Just (trimWhitespace userEntry)

trimWhitespace :: String -> String
trimWhitespace = dropWhileEnd isSpace . dropWhile isSpace
