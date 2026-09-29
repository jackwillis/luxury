module Main where

import Test.Hspec (hspec)

import qualified EnvSpec
import qualified EvalSpec
import qualified ReaderSpec
import qualified SExprSpec
import qualified ValueSpec

main :: IO ()
main = hspec $ do
  EnvSpec.spec
  EvalSpec.spec
  ReaderSpec.spec
  SExprSpec.spec
  ValueSpec.spec
