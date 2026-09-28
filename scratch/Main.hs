module Main where

import System.Console.Haskeline (InputT, defaultSettings, outputStrLn, runInputT)
import qualified SExpr

main :: IO ()
main = runInputT defaultSettings scratchDemo

scratchDemo :: InputT IO ()
scratchDemo = do
  outputStrLn $ SExpr.render $ SExpr.List [SExpr.Symbol "+", SExpr.Number 2, SExpr.Number 3]
  outputStrLn $ SExpr.render $ SExpr.List [SExpr.Symbol "def", SExpr.Symbol "foo", SExpr.Number 2]
  outputStrLn $ SExpr.render $ SExpr.List [
    SExpr.Symbol "println",
      SExpr.List [SExpr.Symbol "+", SExpr.Symbol "foo", SExpr.Number 3]]
