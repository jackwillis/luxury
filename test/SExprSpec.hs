module SExprSpec (spec) where

import Number (Number(..))
import SExpr
import Test.Hspec

spec :: Spec
spec = describe "SExpr.render" $ do
  it "renders a symbol as its name" $
    render (Symbol "foo") `shouldBe` "foo"

  it "renders an exact integer" $
    render (Number (ExactInteger 42)) `shouldBe` "42"

  it "renders an inexact real" $
    render (Number (InexactReal 4.2)) `shouldBe` "4.2"

  it "renders #t for True" $
    render (Boolean True) `shouldBe` "#t"

  it "renders #f for False" $
    render (Boolean False) `shouldBe` "#f"

  it "renders an empty list as ()" $
    render (List []) `shouldBe` "()"

  it "renders a flat list with single spaces between elements" $
    render (List [Symbol "+", Number (ExactInteger 2), Number (ExactInteger 3)])
      `shouldBe` "(+ 2 3)"

  it "renders nested lists" $
    render
      (List
        [ Symbol "println"
        , List [Symbol "+", Symbol "foo", Number (ExactInteger 3)]
        ])
      `shouldBe` "(println (+ foo 3))"
