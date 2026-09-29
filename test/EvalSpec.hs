module EvalSpec (spec) where

import Eval
import Number (Number(..))
import qualified SExpr
import Test.Hspec
import qualified Value

spec :: Spec
spec = describe "Eval.eval" $ do
  it "evaluates an exact integer to itself" $
    eval (SExpr.Number (ExactInteger 42))
      `shouldBe` Right (Value.Number (ExactInteger 42))

  it "evaluates an inexact real to itself" $
    eval (SExpr.Number (InexactReal 4.2))
      `shouldBe` Right (Value.Number (InexactReal 4.2))

  it "evaluates #t to true" $
    eval (SExpr.Boolean True) `shouldBe` Right (Value.Boolean True)

  it "evaluates #f to false" $
    eval (SExpr.Boolean False) `shouldBe` Right (Value.Boolean False)

  it "cannot yet evaluate a symbol" $
    eval (SExpr.Symbol "foo")
      `shouldBe` Left (CannotEvaluate (SExpr.Symbol "foo"))

  it "cannot yet evaluate an empty list" $
    eval (SExpr.List [])
      `shouldBe` Left (CannotEvaluate (SExpr.List []))

  it "cannot yet evaluate a non-empty list" $
    let form = SExpr.List [SExpr.Symbol "+", SExpr.Number (ExactInteger 1)]
    in eval form `shouldBe` Left (CannotEvaluate form)
