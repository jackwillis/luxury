module Main where

import Control.Monad (unless)
import Data.Char (isSpace)
import Data.List (dropWhileEnd)
import System.IO (hFlush, isEOF, stdout)

main :: IO ()
main = do
  loop

loop :: IO ()
loop = do
  showPrompt
  eof <- isEOF
  if eof
    then quit
    else do
      userInput <- getLine
      case toMaybeLine userInput of
        Just line -> putStrLn line
        Nothing   -> pure ()
      loop

showPrompt :: IO ()
showPrompt = do
  putStr "luxury> "
  hFlush stdout

quit :: IO ()
quit = do
  putStrLn "\nGoodbye"

toMaybeLine :: String -> Maybe String
toMaybeLine userEntry
  | all isSpace userEntry = Nothing
  | otherwise             = Just (trimWhitespace userEntry)

trimWhitespace :: String -> String
trimWhitespace = dropWhileEnd isSpace . dropWhile isSpace
