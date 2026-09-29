module Main where

import qualified EvalSpec
import qualified ReaderSpec
import qualified SExprSpec
import Test.Hspec (hspec)
import qualified ValueSpec

main :: IO ()
main = hspec $ do
  SExprSpec.spec
  ReaderSpec.spec
  ValueSpec.spec
  EvalSpec.spec
