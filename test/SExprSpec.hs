module SExprSpec (spec) where

import SExpr
import Test.Hspec

spec :: Spec
spec = describe "SExpr.render" $ do
  it "renders a symbol as its name" $
    render (Symbol "foo") `shouldBe` "foo"

  it "renders a number" $
    render (Number 42) `shouldBe` "42"

  it "renders an empty list as ()" $
    render (List []) `shouldBe` "()"

  it "renders a flat list with single spaces between elements" $
    render (List [Symbol "+", Number 2, Number 3]) `shouldBe` "(+ 2 3)"

  it "renders nested lists" $
    render
      (List
        [ Symbol "println"
        , List [Symbol "+", Symbol "foo", Number 3]
        ])
      `shouldBe` "(println (+ foo 3))"
