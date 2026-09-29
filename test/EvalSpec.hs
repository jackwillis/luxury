module EvalSpec (spec) where

import Eval
import Number (Number(..))
import qualified SExpr
import Test.Hspec
import qualified Value

spec :: Spec
spec = describe "Eval.eval" $ do
  describe "literals" $ do
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

  describe "symbols" $ do
    it "cannot yet evaluate a bare symbol (needs environment)" $
      eval (SExpr.Symbol "foo")
        `shouldBe` Left (CannotEvaluate (SExpr.Symbol "foo"))

  describe "arithmetic operations" $ do
    it "evaluates (+ 1 2) to 3" $
      eval (SExpr.List [SExpr.Symbol "+", SExpr.Number (ExactInteger 1), SExpr.Number (ExactInteger 2)])
        `shouldBe` Right (Value.Number (ExactInteger 3))

    it "evaluates (+ 1 2 3) to 6" $
      eval (SExpr.List [SExpr.Symbol "+", SExpr.Number (ExactInteger 1), SExpr.Number (ExactInteger 2), SExpr.Number (ExactInteger 3)])
        `shouldBe` Right (Value.Number (ExactInteger 6))

    it "evaluates (- 5 3) to 2" $
      eval (SExpr.List [SExpr.Symbol "-", SExpr.Number (ExactInteger 5), SExpr.Number (ExactInteger 3)])
        `shouldBe` Right (Value.Number (ExactInteger 2))

    it "evaluates (* 2 3) to 6" $
      eval (SExpr.List [SExpr.Symbol "*", SExpr.Number (ExactInteger 2), SExpr.Number (ExactInteger 3)])
        `shouldBe` Right (Value.Number (ExactInteger 6))

    it "evaluates (/ 6 2) to 3" $
      eval (SExpr.List [SExpr.Symbol "/", SExpr.Number (ExactInteger 6), SExpr.Number (ExactInteger 2)])
        `shouldBe` Right (Value.Number (ExactInteger 3))

  describe "comparison operations" $ do
    it "evaluates (= 1 1) to #t" $
      eval (SExpr.List [SExpr.Symbol "=", SExpr.Number (ExactInteger 1), SExpr.Number (ExactInteger 1)])
        `shouldBe` Right (Value.Boolean True)

    it "evaluates (= 1 2) to #f" $
      eval (SExpr.List [SExpr.Symbol "=", SExpr.Number (ExactInteger 1), SExpr.Number (ExactInteger 2)])
        `shouldBe` Right (Value.Boolean False)

    it "evaluates (< 1 2) to #t" $
      eval (SExpr.List [SExpr.Symbol "<", SExpr.Number (ExactInteger 1), SExpr.Number (ExactInteger 2)])
        `shouldBe` Right (Value.Boolean True)

    it "evaluates (> 2 1) to #t" $
      eval (SExpr.List [SExpr.Symbol ">", SExpr.Number (ExactInteger 2), SExpr.Number (ExactInteger 1)])
        `shouldBe` Right (Value.Boolean True)

  describe "special forms" $ do
    it "evaluates (quote x) to the symbol x" $
      eval (SExpr.List [SExpr.Symbol "quote", SExpr.Symbol "x"])
        `shouldBe` Right (Value.Symbol "x")

    it "evaluates (quote 42) to 42" $
      eval (SExpr.List [SExpr.Symbol "quote", SExpr.Number (ExactInteger 42)])
        `shouldBe` Right (Value.Number (ExactInteger 42))

    it "evaluates (if #t 1 2) to 1" $
      eval (SExpr.List [SExpr.Symbol "if", SExpr.Boolean True, SExpr.Number (ExactInteger 1), SExpr.Number (ExactInteger 2)])
        `shouldBe` Right (Value.Number (ExactInteger 1))

    it "evaluates (if #f 1 2) to 2" $
      eval (SExpr.List [SExpr.Symbol "if", SExpr.Boolean False, SExpr.Number (ExactInteger 1), SExpr.Number (ExactInteger 2)])
        `shouldBe` Right (Value.Number (ExactInteger 2))

  describe "nested evaluation" $ do
    it "evaluates nested arithmetic: (+ 1 (+ 2 3))" $
      eval (SExpr.List [SExpr.Symbol "+", SExpr.Number (ExactInteger 1),
                        SExpr.List [SExpr.Symbol "+", SExpr.Number (ExactInteger 2), SExpr.Number (ExactInteger 3)]])
        `shouldBe` Right (Value.Number (ExactInteger 6))

    it "evaluates deeply nested arithmetic: (+ (+ 1 2) (+ 3 4))" $
      eval (SExpr.List [SExpr.Symbol "+",
                        SExpr.List [SExpr.Symbol "+", SExpr.Number (ExactInteger 1), SExpr.Number (ExactInteger 2)],
                        SExpr.List [SExpr.Symbol "+", SExpr.Number (ExactInteger 3), SExpr.Number (ExactInteger 4)]])
        `shouldBe` Right (Value.Number (ExactInteger 10))

    it "evaluates arithmetic with symbolic result: (* (+ 1 2) (- 5 2))" $
      eval (SExpr.List [SExpr.Symbol "*",
                        SExpr.List [SExpr.Symbol "+", SExpr.Number (ExactInteger 1), SExpr.Number (ExactInteger 2)],
                        SExpr.List [SExpr.Symbol "-", SExpr.Number (ExactInteger 5), SExpr.Number (ExactInteger 2)]])
        `shouldBe` Right (Value.Number (ExactInteger 9))

  describe "arity errors" $ do
    it "fails on + with no arguments" $
      eval (SExpr.List [SExpr.Symbol "+"])
        `shouldBe` Left (CannotEvaluate (SExpr.List [SExpr.Symbol "+"]))

    it "fails on + with one argument" $
      eval (SExpr.List [SExpr.Symbol "+", SExpr.Number (ExactInteger 1)])
        `shouldBe` Left (CannotEvaluate (SExpr.List [SExpr.Symbol "+", SExpr.Number (ExactInteger 1)]))

    it "fails on - with no arguments" $
      eval (SExpr.List [SExpr.Symbol "-"])
        `shouldBe` Left (CannotEvaluate (SExpr.List [SExpr.Symbol "-"]))

    it "fails on * with no arguments" $
      eval (SExpr.List [SExpr.Symbol "*"])
        `shouldBe` Left (CannotEvaluate (SExpr.List [SExpr.Symbol "*"]))

    it "fails on / with one argument" $
      eval (SExpr.List [SExpr.Symbol "/", SExpr.Number (ExactInteger 6)])
        `shouldBe` Left (CannotEvaluate (SExpr.List [SExpr.Symbol "/", SExpr.Number (ExactInteger 6)]))

    it "fails on = with no arguments" $
      eval (SExpr.List [SExpr.Symbol "="])
        `shouldBe` Left (CannotEvaluate (SExpr.List [SExpr.Symbol "="]))

    it "fails on < with one argument" $
      eval (SExpr.List [SExpr.Symbol "<", SExpr.Number (ExactInteger 1)])
        `shouldBe` Left (CannotEvaluate (SExpr.List [SExpr.Symbol "<", SExpr.Number (ExactInteger 1)]))

  describe "type errors" $ do
    it "fails on + with string operand" $
      eval (SExpr.List [SExpr.Symbol "+", SExpr.Symbol "a", SExpr.Number (ExactInteger 1)])
        `shouldBe` Left (CannotEvaluate (SExpr.List [SExpr.Symbol "+", SExpr.Symbol "a", SExpr.Number (ExactInteger 1)]))

    it "fails on - with boolean operand" $
      eval (SExpr.List [SExpr.Symbol "-", SExpr.Boolean True, SExpr.Number (ExactInteger 1)])
        `shouldBe` Left (CannotEvaluate (SExpr.List [SExpr.Symbol "-", SExpr.Boolean True, SExpr.Number (ExactInteger 1)]))

    it "fails on < with symbol operands" $
      eval (SExpr.List [SExpr.Symbol "<", SExpr.Symbol "a", SExpr.Symbol "b"])
        `shouldBe` Left (CannotEvaluate (SExpr.List [SExpr.Symbol "<", SExpr.Symbol "a", SExpr.Symbol "b"]))

    it "fails on = comparing number and boolean" $
      eval (SExpr.List [SExpr.Symbol "=", SExpr.Number (ExactInteger 1), SExpr.Boolean True])
        `shouldBe` Left (CannotEvaluate (SExpr.List [SExpr.Symbol "=", SExpr.Number (ExactInteger 1), SExpr.Boolean True]))

  describe "malformed special forms" $ do
    it "fails on if with no branches" $
      eval (SExpr.List [SExpr.Symbol "if"])
        `shouldBe` Left (CannotEvaluate (SExpr.List [SExpr.Symbol "if"]))

    it "fails on if with only condition" $
      eval (SExpr.List [SExpr.Symbol "if", SExpr.Boolean True])
        `shouldBe` Left (CannotEvaluate (SExpr.List [SExpr.Symbol "if", SExpr.Boolean True]))

    it "fails on if with only then branch" $
      eval (SExpr.List [SExpr.Symbol "if", SExpr.Boolean True, SExpr.Number (ExactInteger 1)])
        `shouldBe` Left (CannotEvaluate (SExpr.List [SExpr.Symbol "if", SExpr.Boolean True, SExpr.Number (ExactInteger 1)]))

    it "fails on if with too many arguments" $
      eval (SExpr.List [SExpr.Symbol "if", SExpr.Boolean True, SExpr.Number (ExactInteger 1), SExpr.Number (ExactInteger 2), SExpr.Number (ExactInteger 3)])
        `shouldBe` Left (CannotEvaluate (SExpr.List [SExpr.Symbol "if", SExpr.Boolean True, SExpr.Number (ExactInteger 1), SExpr.Number (ExactInteger 2), SExpr.Number (ExactInteger 3)]))

    it "fails on quote with no argument" $
      eval (SExpr.List [SExpr.Symbol "quote"])
        `shouldBe` Left (CannotEvaluate (SExpr.List [SExpr.Symbol "quote"]))

    it "fails on quote with multiple arguments" $
      eval (SExpr.List [SExpr.Symbol "quote", SExpr.Number (ExactInteger 1), SExpr.Number (ExactInteger 2)])
        `shouldBe` Left (CannotEvaluate (SExpr.List [SExpr.Symbol "quote", SExpr.Number (ExactInteger 1), SExpr.Number (ExactInteger 2)]))

  describe "non-callable operators" $ do
    it "fails when number is in operator position: (1 2 3)" $
      eval (SExpr.List [SExpr.Number (ExactInteger 1), SExpr.Number (ExactInteger 2), SExpr.Number (ExactInteger 3)])
        `shouldBe` Left (CannotEvaluate (SExpr.List [SExpr.Number (ExactInteger 1), SExpr.Number (ExactInteger 2), SExpr.Number (ExactInteger 3)]))

    it "fails when boolean is in operator position: (#t 1 2)" $
      eval (SExpr.List [SExpr.Boolean True, SExpr.Number (ExactInteger 1), SExpr.Number (ExactInteger 2)])
        `shouldBe` Left (CannotEvaluate (SExpr.List [SExpr.Boolean True, SExpr.Number (ExactInteger 1), SExpr.Number (ExactInteger 2)]))

    it "fails when list is in operator position: ((+ 1 2) 3)" $
      eval (SExpr.List [SExpr.List [SExpr.Symbol "+", SExpr.Number (ExactInteger 1), SExpr.Number (ExactInteger 2)], SExpr.Number (ExactInteger 3)])
        `shouldBe` Left (CannotEvaluate (SExpr.List [SExpr.List [SExpr.Symbol "+", SExpr.Number (ExactInteger 1), SExpr.Number (ExactInteger 2)], SExpr.Number (ExactInteger 3)]))

  describe "numeric edge cases" $ do
    it "evaluates zero" $
      eval (SExpr.Number (ExactInteger 0))
        `shouldBe` Right (Value.Number (ExactInteger 0))

    it "evaluates negative numbers" $
      eval (SExpr.Number (ExactInteger (-42)))
        `shouldBe` Right (Value.Number (ExactInteger (-42)))

    it "evaluates floating point zero" $
      eval (SExpr.Number (InexactReal 0.0))
        `shouldBe` Right (Value.Number (InexactReal 0.0))

    it "evaluates negative floats" $
      eval (SExpr.Number (InexactReal (-3.14)))
        `shouldBe` Right (Value.Number (InexactReal (-3.14)))

    it "handles mixed integer/float arithmetic: (+ 1 2.5)" $
      eval (SExpr.List [SExpr.Symbol "+", SExpr.Number (ExactInteger 1), SExpr.Number (InexactReal 2.5)])
        `shouldBe` Right (Value.Number (InexactReal 3.5))

    it "handles division resulting in float: (/ 5 2)" $
      eval (SExpr.List [SExpr.Symbol "/", SExpr.Number (ExactInteger 5), SExpr.Number (ExactInteger 2)])
        `shouldBe` Right (Value.Number (InexactReal 2.5))

  describe "errors" $ do
    it "fails on an empty list" $
      eval (SExpr.List [])
        `shouldBe` Left (CannotEvaluate (SExpr.List []))

    it "fails on unknown built-in" $
      let form = SExpr.List [SExpr.Symbol "unknown-op", SExpr.Number (ExactInteger 1)]
      in eval form `shouldBe` Left (CannotEvaluate form)

    it "fails on division by zero" $
      eval (SExpr.List [SExpr.Symbol "/", SExpr.Number (ExactInteger 1), SExpr.Number (ExactInteger 0)])
        `shouldBe` Left (CannotEvaluate (SExpr.List [SExpr.Symbol "/", SExpr.Number (ExactInteger 1), SExpr.Number (ExactInteger 0)]))
