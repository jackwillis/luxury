module Main where

import System.Console.Haskeline (InputT, defaultSettings, outputStrLn, runInputT)
import Number (Number(..))
import SExpr (SExpr(..), render)
import qualified Eval
import qualified Reader
import qualified Value

main :: IO ()
main = runInputT defaultSettings scratchDemo

scratchDemo :: InputT IO ()
scratchDemo = do
  banner "S-expressions"
  outputStrLn $ render $ List [Symbol "+", Number (ExactInteger 2), Number (ExactInteger 3)]
  outputStrLn $ render $ List [Symbol "def", Symbol "foo", Number (ExactInteger 2)]
  outputStrLn $ render $ List [
    Symbol "println",
      List [Symbol "+", Symbol "foo", Number (ExactInteger 3)]]
  outputStrLn $ render $ List [Symbol "+", Number (InexactReal 1.5), Boolean True]

  banner "tokens"
  showTokens "(+ 2 3)"
  showTokens "(def foo 2) foo"
  showTokens "(a (b c))"

  banner "reading"
  showReadProgram "(+ 2 3)"
  showReadProgram "42 #t 1.5"
  showReadProgram "(foo"
  showReadProgram "foo )"

  banner "evaluation"
  showEval $ Number (ExactInteger 7)
  showEval $ Number (InexactReal 2.5)
  showEval $ Boolean False
  showEval $ List [Symbol "+", Number (ExactInteger 1), Number (ExactInteger 2)]
  showEval $ Symbol "foo"

showTokens :: String -> InputT IO ()
showTokens source =
  outputStrLn $ "tokenize " <> show source <> " = " <> show (Reader.tokenize source)

showReadProgram :: String -> InputT IO ()
showReadProgram source =
  outputStrLn $ "readProgram " <> show source <> " = " <> case Reader.readProgram source of
    Left readerError  -> Reader.renderReaderError readerError
    Right expressions -> show expressions

showEval :: SExpr -> InputT IO ()
showEval expression =
  outputStrLn $ "eval " <> show expression <> " = " <> case Eval.eval expression of
    Left evalError -> show evalError
    Right value    -> Value.render value


banner :: String -> InputT IO ()
banner message = do
  outputStrLn ""
  outputStrLn message
  outputStrLn $ take (length message) $ repeat '='
  outputStrLn ""
