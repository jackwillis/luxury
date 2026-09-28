module Main where

import System.Console.Haskeline (InputT, defaultSettings, outputStrLn, runInputT)
import SExpr (SExpr(..), render)
import qualified Reader

main :: IO ()
main = runInputT defaultSettings scratchDemo

scratchDemo :: InputT IO ()
scratchDemo = do
  banner "S-expressions"
  outputStrLn $ render $ List [Symbol "+", Number 2, Number 3]
  outputStrLn $ render $ List [Symbol "def", Symbol "foo", Number 2]
  outputStrLn $ render $ List [
    Symbol "println",
      List [Symbol "+", Symbol "foo", Number 3]]

  banner "tokens"
  showTokens "(+ 2 3)"
  showTokens "(def foo 2) foo"
  showTokens "(a (b c))"

showTokens :: String -> InputT IO ()
showTokens source =
  outputStrLn $ "tokenize " <> show source <> " = " <> show (Reader.tokenize source)


banner :: String -> InputT IO ()
banner message = do
  outputStrLn ""
  outputStrLn message
  outputStrLn $ take (length message) $ repeat '='
  outputStrLn ""
