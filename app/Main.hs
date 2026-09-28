module Main where

import Data.Char (isSpace)
import Data.List (dropWhileEnd)
import System.Console.Haskeline (InputT, getInputLine, outputStrLn, runInputT)
import Text.Printf (printf)
import qualified SExpr
import qualified System.Console.Haskeline as Haskeline

main :: IO ()
main = do
  runInputT Haskeline.defaultSettings $ do
    scratchDemo
    loop 1

scratchDemo :: InputT IO ()
scratchDemo = do
  let sExpr = SExpr.List [SExpr.Symbol "+", SExpr.Number 2, SExpr.Number 3]

  outputStrLn $ show sExpr
  outputStrLn $ SExpr.render sExpr

loop :: Int -> InputT IO ()
loop promptCount = do
  let prompt = printf "luxury(%03d)> " promptCount
  lineOrEof <- getInputLine prompt
  case lineOrEof of
    Nothing -> outputStrLn "Goodbye"
    Just userInput -> do
      case toMaybeLine userInput of
        Just line -> outputStrLn line
        Nothing   -> pure ()
      loop (promptCount + 1)

toMaybeLine :: String -> Maybe String
toMaybeLine userEntry
  | all isSpace userEntry = Nothing
  | otherwise             = Just (trimWhitespace userEntry)

trimWhitespace :: String -> String
trimWhitespace = dropWhileEnd isSpace . dropWhile isSpace
