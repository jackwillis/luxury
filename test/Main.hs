module Main where

import qualified SExprSpec
import Test.Hspec (hspec)

main :: IO ()
main = hspec SExprSpec.spec
