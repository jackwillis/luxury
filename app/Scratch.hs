module Main where

import System.Console.Haskeline (InputT, defaultSettings, outputStrLn, runInputT)
import qualified SExpr

main :: IO ()
main = runInputT defaultSettings scratchDemo

scratchDemo :: InputT IO ()
scratchDemo = do
  let sExpr = SExpr.List [SExpr.Symbol "+", SExpr.Number 2, SExpr.Number 3]

  outputStrLn $ show sExpr
  outputStrLn $ SExpr.render sExpr
