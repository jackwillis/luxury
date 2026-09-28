module Main where

import qualified ReaderSpec
import qualified SExprSpec
import Test.Hspec (hspec)

main :: IO ()
main = hspec $ do
  SExprSpec.spec
  ReaderSpec.spec
