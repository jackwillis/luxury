module Main where

import Data.Char (isSpace)
import Data.List (dropWhileEnd)
import System.Console.Haskeline

main :: IO ()
main = runInputT defaultSettings loop

loop :: InputT IO ()
loop = do
  minput <- getInputLine "luxury> "
  case minput of
    Nothing -> outputStrLn "Goodbye"
    Just userInput -> do
      case toMaybeLine userInput of
        Just line -> outputStrLn line
        Nothing   -> pure ()
      loop

toMaybeLine :: String -> Maybe String
toMaybeLine userEntry
  | all isSpace userEntry = Nothing
  | otherwise             = Just (trimWhitespace userEntry)

trimWhitespace :: String -> String
trimWhitespace = dropWhileEnd isSpace . dropWhile isSpace
