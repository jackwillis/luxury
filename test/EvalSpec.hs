module EvalSpec (spec) where

import qualified Env
import Eval
import Number (Number(..))
import qualified SExpr
import Test.Hspec
import qualified Value

evalEmpty :: SExpr.SExpr -> Either EvalError Value.Value
evalEmpty =
  eval Env.empty

spec :: Spec
spec = describe "Eval.eval" $ do
  describe "literals" $ do
    it "evaluates an exact integer to itself" $
      evalEmpty (SExpr.Number (ExactInteger 42))
        `shouldBe` Right (Value.Number (ExactInteger 42))

    it "evaluates an inexact real to itself" $
      evalEmpty (SExpr.Number (InexactReal 4.2))
        `shouldBe` Right (Value.Number (InexactReal 4.2))

    it "evaluates #t to true" $
      evalEmpty (SExpr.Boolean True)
        `shouldBe` Right (Value.Boolean True)

    it "evaluates #f to false" $
      evalEmpty (SExpr.Boolean False)
        `shouldBe` Right (Value.Boolean False)

  describe "symbols" $ do
    it "fails on an unbound symbol" $
      evalEmpty (SExpr.Symbol "foo")
        `shouldBe` Left (UnboundVariable "foo")

    it "evaluates a bound symbol to its value" $
      let environment =
            Env.fromList
              [ ("foo", Value.Number (ExactInteger 42))
              ]
      in eval environment (SExpr.Symbol "foo")
           `shouldBe` Right (Value.Number (ExactInteger 42))

    it "can bind a symbol to a non-number value" $
      let environment =
            Env.fromList
              [ ("answer", Value.Boolean True)
              ]
      in eval environment (SExpr.Symbol "answer")
           `shouldBe` Right (Value.Boolean True)

  describe "arithmetic operations" $ do
    it "evaluates (+ 1 2) to 3" $
      evalEmpty
        (SExpr.List
          [ SExpr.Symbol "+"
          , SExpr.Number (ExactInteger 1)
          , SExpr.Number (ExactInteger 2)
          ])
        `shouldBe` Right (Value.Number (ExactInteger 3))

    it "evaluates (+ 1 2 3) to 6" $
      evalEmpty
        (SExpr.List
          [ SExpr.Symbol "+"
          , SExpr.Number (ExactInteger 1)
          , SExpr.Number (ExactInteger 2)
          , SExpr.Number (ExactInteger 3)
          ])
        `shouldBe` Right (Value.Number (ExactInteger 6))

    it "evaluates (- 5 3) to 2" $
      evalEmpty
        (SExpr.List
          [ SExpr.Symbol "-"
          , SExpr.Number (ExactInteger 5)
          , SExpr.Number (ExactInteger 3)
          ])
        `shouldBe` Right (Value.Number (ExactInteger 2))

    it "evaluates (* 2 3) to 6" $
      evalEmpty
        (SExpr.List
          [ SExpr.Symbol "*"
          , SExpr.Number (ExactInteger 2)
          , SExpr.Number (ExactInteger 3)
          ])
        `shouldBe` Right (Value.Number (ExactInteger 6))

    it "evaluates (/ 6 2) to 3" $
      evalEmpty
        (SExpr.List
          [ SExpr.Symbol "/"
          , SExpr.Number (ExactInteger 6)
          , SExpr.Number (ExactInteger 2)
          ])
        `shouldBe` Right (Value.Number (ExactInteger 3))

  describe "comparison operations" $ do
    describe "binary comparisons" $ do
      it "evaluates (= 1 1) to #t" $
        evalEmpty
          (SExpr.List
            [ SExpr.Symbol "="
            , SExpr.Number (ExactInteger 1)
            , SExpr.Number (ExactInteger 1)
            ])
          `shouldBe` Right (Value.Boolean True)

      it "evaluates (= 1 2) to #f" $
        evalEmpty
          (SExpr.List
            [ SExpr.Symbol "="
            , SExpr.Number (ExactInteger 1)
            , SExpr.Number (ExactInteger 2)
            ])
          `shouldBe` Right (Value.Boolean False)

      it "evaluates (< 1 2) to #t" $
        evalEmpty
          (SExpr.List
            [ SExpr.Symbol "<"
            , SExpr.Number (ExactInteger 1)
            , SExpr.Number (ExactInteger 2)
            ])
          `shouldBe` Right (Value.Boolean True)

      it "evaluates (> 2 1) to #t" $
        evalEmpty
          (SExpr.List
            [ SExpr.Symbol ">"
            , SExpr.Number (ExactInteger 2)
            , SExpr.Number (ExactInteger 1)
            ])
          `shouldBe` Right (Value.Boolean True)

    describe "variadic comparisons (R7RS)" $ do
      it "evaluates (< 1 2 3) to #t" $
        evalEmpty
          (SExpr.List
            [ SExpr.Symbol "<"
            , SExpr.Number (ExactInteger 1)
            , SExpr.Number (ExactInteger 2)
            , SExpr.Number (ExactInteger 3)
            ])
          `shouldBe` Right (Value.Boolean True)

      it "evaluates (< 1 3 2) to #f" $
        evalEmpty
          (SExpr.List
            [ SExpr.Symbol "<"
            , SExpr.Number (ExactInteger 1)
            , SExpr.Number (ExactInteger 3)
            , SExpr.Number (ExactInteger 2)
            ])
          `shouldBe` Right (Value.Boolean False)

      it "evaluates (> 3 2 1) to #t" $
        evalEmpty
          (SExpr.List
            [ SExpr.Symbol ">"
            , SExpr.Number (ExactInteger 3)
            , SExpr.Number (ExactInteger 2)
            , SExpr.Number (ExactInteger 1)
            ])
          `shouldBe` Right (Value.Boolean True)

      it "evaluates (= 5 5 5) to #t" $
        evalEmpty
          (SExpr.List
            [ SExpr.Symbol "="
            , SExpr.Number (ExactInteger 5)
            , SExpr.Number (ExactInteger 5)
            , SExpr.Number (ExactInteger 5)
            ])
          `shouldBe` Right (Value.Boolean True)

  describe "special forms" $ do
    describe "quote" $ do
      it "evaluates (quote x) to the symbol x" $
        evalEmpty
          (SExpr.List
            [ SExpr.Symbol "quote"
            , SExpr.Symbol "x"
            ])
          `shouldBe` Right (Value.Symbol "x")

      it "evaluates (quote 42) to 42" $
        evalEmpty
          (SExpr.List
            [ SExpr.Symbol "quote"
            , SExpr.Number (ExactInteger 42)
            ])
          `shouldBe` Right (Value.Number (ExactInteger 42))

      it "evaluates (quote #t) to #t" $
        evalEmpty
          (SExpr.List
            [ SExpr.Symbol "quote"
            , SExpr.Boolean True
            ])
          `shouldBe` Right (Value.Boolean True)

      it "evaluates (quote ()) to empty list" $
        evalEmpty
          (SExpr.List
            [ SExpr.Symbol "quote"
            , SExpr.List []
            ])
          `shouldBe` Right (Value.List [])

      it "evaluates (quote (a b c)) to list of symbols" $
        evalEmpty
          (SExpr.List
            [ SExpr.Symbol "quote"
            , SExpr.List
                [ SExpr.Symbol "a"
                , SExpr.Symbol "b"
                , SExpr.Symbol "c"
                ]
            ])
          `shouldBe`
            Right
              (Value.List
                [ Value.Symbol "a"
                , Value.Symbol "b"
                , Value.Symbol "c"
                ])

      it "evaluates (quote (1 2 3)) to list of numbers" $
        evalEmpty
          (SExpr.List
            [ SExpr.Symbol "quote"
            , SExpr.List
                [ SExpr.Number (ExactInteger 1)
                , SExpr.Number (ExactInteger 2)
                , SExpr.Number (ExactInteger 3)
                ]
            ])
          `shouldBe`
            Right
              (Value.List
                [ Value.Number (ExactInteger 1)
                , Value.Number (ExactInteger 2)
                , Value.Number (ExactInteger 3)
                ])

      it "evaluates (quote (a (b c) d)) to nested list structure" $
        evalEmpty
          (SExpr.List
            [ SExpr.Symbol "quote"
            , SExpr.List
                [ SExpr.Symbol "a"
                , SExpr.List
                    [ SExpr.Symbol "b"
                    , SExpr.Symbol "c"
                    ]
                , SExpr.Symbol "d"
                ]
            ])
          `shouldBe`
            Right
              (Value.List
                [ Value.Symbol "a"
                , Value.List
                    [ Value.Symbol "b"
                    , Value.Symbol "c"
                    ]
                , Value.Symbol "d"
                ])

      it "evaluates (quote (+ 1 2)) to unevaluated form" $
        evalEmpty
          (SExpr.List
            [ SExpr.Symbol "quote"
            , SExpr.List
                [ SExpr.Symbol "+"
                , SExpr.Number (ExactInteger 1)
                , SExpr.Number (ExactInteger 2)
                ]
            ])
          `shouldBe`
            Right
              (Value.List
                [ Value.Symbol "+"
                , Value.Number (ExactInteger 1)
                , Value.Number (ExactInteger 2)
                ])

    describe "if" $ do
      it "evaluates (if #t 1 2) to 1" $
        evalEmpty
          (SExpr.List
            [ SExpr.Symbol "if"
            , SExpr.Boolean True
            , SExpr.Number (ExactInteger 1)
            , SExpr.Number (ExactInteger 2)
            ])
          `shouldBe` Right (Value.Number (ExactInteger 1))

      it "evaluates (if #f 1 2) to 2" $
        evalEmpty
          (SExpr.List
            [ SExpr.Symbol "if"
            , SExpr.Boolean False
            , SExpr.Number (ExactInteger 1)
            , SExpr.Number (ExactInteger 2)
            ])
          `shouldBe` Right (Value.Number (ExactInteger 2))

  describe "nested evaluation" $ do
    it "evaluates nested arithmetic: (+ 1 (+ 2 3))" $
      evalEmpty
        (SExpr.List
          [ SExpr.Symbol "+"
          , SExpr.Number (ExactInteger 1)
          , SExpr.List
              [ SExpr.Symbol "+"
              , SExpr.Number (ExactInteger 2)
              , SExpr.Number (ExactInteger 3)
              ]
          ])
        `shouldBe` Right (Value.Number (ExactInteger 6))

    it "evaluates deeply nested arithmetic: (+ (+ 1 2) (+ 3 4))" $
      evalEmpty
        (SExpr.List
          [ SExpr.Symbol "+"
          , SExpr.List
              [ SExpr.Symbol "+"
              , SExpr.Number (ExactInteger 1)
              , SExpr.Number (ExactInteger 2)
              ]
          , SExpr.List
              [ SExpr.Symbol "+"
              , SExpr.Number (ExactInteger 3)
              , SExpr.Number (ExactInteger 4)
              ]
          ])
        `shouldBe` Right (Value.Number (ExactInteger 10))

    it "evaluates arithmetic with nested results: (* (+ 1 2) (- 5 2))" $
      evalEmpty
        (SExpr.List
          [ SExpr.Symbol "*"
          , SExpr.List
              [ SExpr.Symbol "+"
              , SExpr.Number (ExactInteger 1)
              , SExpr.Number (ExactInteger 2)
              ]
          , SExpr.List
              [ SExpr.Symbol "-"
              , SExpr.Number (ExactInteger 5)
              , SExpr.Number (ExactInteger 2)
              ]
          ])
        `shouldBe` Right (Value.Number (ExactInteger 9))

  describe "arity and variadic operations (R7RS)" $ do
    it "evaluates (+) to 0 (identity)" $
      evalEmpty (SExpr.List [SExpr.Symbol "+"])
        `shouldBe` Right (Value.Number (ExactInteger 0))

    it "evaluates (+ 5) to 5" $
      evalEmpty
        (SExpr.List
          [ SExpr.Symbol "+"
          , SExpr.Number (ExactInteger 5)
          ])
        `shouldBe` Right (Value.Number (ExactInteger 5))

    it "evaluates (*) to 1 (identity)" $
      evalEmpty (SExpr.List [SExpr.Symbol "*"])
        `shouldBe` Right (Value.Number (ExactInteger 1))

    it "evaluates (* 5) to 5" $
      evalEmpty
        (SExpr.List
          [ SExpr.Symbol "*"
          , SExpr.Number (ExactInteger 5)
          ])
        `shouldBe` Right (Value.Number (ExactInteger 5))

    it "evaluates (- 5) to -5 (unary negation)" $
      evalEmpty
        (SExpr.List
          [ SExpr.Symbol "-"
          , SExpr.Number (ExactInteger 5)
          ])
        `shouldBe` Right (Value.Number (ExactInteger (-5)))

    it "evaluates (/ 2) to 0.5 (reciprocal)" $
      evalEmpty
        (SExpr.List
          [ SExpr.Symbol "/"
          , SExpr.Number (ExactInteger 2)
          ])
        `shouldBe` Right (Value.Number (InexactReal 0.5))

    it "fails on - with no arguments" $
      evalEmpty (SExpr.List [SExpr.Symbol "-"])
        `shouldBe` Left (CannotEvaluate (SExpr.List [SExpr.Symbol "-"]))

    it "fails on / with no arguments" $
      evalEmpty (SExpr.List [SExpr.Symbol "/"])
        `shouldBe` Left (CannotEvaluate (SExpr.List [SExpr.Symbol "/"]))

  describe "type errors" $ do
    it "fails on + with symbol operand" $
      let form =
            SExpr.List
              [ SExpr.Symbol "+"
              , SExpr.List
                  [ SExpr.Symbol "quote"
                  , SExpr.Symbol "a"
                  ]
              , SExpr.Number (ExactInteger 1)
              ]
      in evalEmpty form
           `shouldBe` Left (CannotEvaluate form)

    it "fails on - with boolean operand" $
      let form =
            SExpr.List
              [ SExpr.Symbol "-"
              , SExpr.Boolean True
              , SExpr.Number (ExactInteger 1)
              ]
      in evalEmpty form
           `shouldBe` Left (CannotEvaluate form)

    it "fails on < with symbol operands" $
      let form =
            SExpr.List
              [ SExpr.Symbol "<"
              , SExpr.List
                  [ SExpr.Symbol "quote"
                  , SExpr.Symbol "a"
                  ]
              , SExpr.List
                  [ SExpr.Symbol "quote"
                  , SExpr.Symbol "b"
                  ]
              ]
      in evalEmpty form
           `shouldBe` Left (CannotEvaluate form)

    it "fails on = comparing number and boolean" $
      let form =
            SExpr.List
              [ SExpr.Symbol "="
              , SExpr.Number (ExactInteger 1)
              , SExpr.Boolean True
              ]
      in evalEmpty form
           `shouldBe` Left (CannotEvaluate form)

  describe "malformed special forms" $ do
    it "fails on if with no branches" $
      evalEmpty (SExpr.List [SExpr.Symbol "if"])
        `shouldBe` Left (CannotEvaluate (SExpr.List [SExpr.Symbol "if"]))

    it "fails on if with only condition" $
      evalEmpty
        (SExpr.List
          [ SExpr.Symbol "if"
          , SExpr.Boolean True
          ])
        `shouldBe`
          Left
            (CannotEvaluate
              (SExpr.List
                [ SExpr.Symbol "if"
                , SExpr.Boolean True
                ]))

    it "fails on if with only then branch" $
      evalEmpty
        (SExpr.List
          [ SExpr.Symbol "if"
          , SExpr.Boolean True
          , SExpr.Number (ExactInteger 1)
          ])
        `shouldBe`
          Left
            (CannotEvaluate
              (SExpr.List
                [ SExpr.Symbol "if"
                , SExpr.Boolean True
                , SExpr.Number (ExactInteger 1)
                ]))

    it "fails on if with too many arguments" $
      let form =
            SExpr.List
              [ SExpr.Symbol "if"
              , SExpr.Boolean True
              , SExpr.Number (ExactInteger 1)
              , SExpr.Number (ExactInteger 2)
              , SExpr.Number (ExactInteger 3)
              ]
      in evalEmpty form
           `shouldBe` Left (CannotEvaluate form)

    it "fails on quote with no argument" $
      evalEmpty (SExpr.List [SExpr.Symbol "quote"])
        `shouldBe` Left (CannotEvaluate (SExpr.List [SExpr.Symbol "quote"]))

    it "fails on quote with multiple arguments" $
      let form =
            SExpr.List
              [ SExpr.Symbol "quote"
              , SExpr.Number (ExactInteger 1)
              , SExpr.Number (ExactInteger 2)
              ]
      in evalEmpty form
           `shouldBe` Left (CannotEvaluate form)

  describe "non-callable operators" $ do
    it "fails when number is in operator position: (1 2 3)" $
      let form =
            SExpr.List
              [ SExpr.Number (ExactInteger 1)
              , SExpr.Number (ExactInteger 2)
              , SExpr.Number (ExactInteger 3)
              ]
      in evalEmpty form
           `shouldBe` Left (CannotEvaluate form)

    it "fails when boolean is in operator position: (#t 1 2)" $
      let form =
            SExpr.List
              [ SExpr.Boolean True
              , SExpr.Number (ExactInteger 1)
              , SExpr.Number (ExactInteger 2)
              ]
      in evalEmpty form
           `shouldBe` Left (CannotEvaluate form)

    it "fails when list is in operator position: ((+ 1 2) 3)" $
      let form =
            SExpr.List
              [ SExpr.List
                  [ SExpr.Symbol "+"
                  , SExpr.Number (ExactInteger 1)
                  , SExpr.Number (ExactInteger 2)
                  ]
              , SExpr.Number (ExactInteger 3)
              ]
      in evalEmpty form
           `shouldBe` Left (CannotEvaluate form)

  describe "numeric edge cases" $ do
    it "evaluates zero" $
      evalEmpty (SExpr.Number (ExactInteger 0))
        `shouldBe` Right (Value.Number (ExactInteger 0))

    it "evaluates negative numbers" $
      evalEmpty (SExpr.Number (ExactInteger (-42)))
        `shouldBe` Right (Value.Number (ExactInteger (-42)))

    it "evaluates floating point zero" $
      evalEmpty (SExpr.Number (InexactReal 0.0))
        `shouldBe` Right (Value.Number (InexactReal 0.0))

    it "evaluates negative floats" $
      evalEmpty (SExpr.Number (InexactReal (-3.14)))
        `shouldBe` Right (Value.Number (InexactReal (-3.14)))

  describe "arithmetic type preservation (R7RS)" $ do
    it "handles mixed integer/float arithmetic: (+ 1 2.5)" $
      evalEmpty
        (SExpr.List
          [ SExpr.Symbol "+"
          , SExpr.Number (ExactInteger 1)
          , SExpr.Number (InexactReal 2.5)
          ])
        `shouldBe` Right (Value.Number (InexactReal 3.5))

    it "preserves exact type for exact operands: (+ 1 2)" $
      evalEmpty
        (SExpr.List
          [ SExpr.Symbol "+"
          , SExpr.Number (ExactInteger 1)
          , SExpr.Number (ExactInteger 2)
          ])
        `shouldBe` Right (Value.Number (ExactInteger 3))

    it "returns exact for exact multiplication: (* 6 7)" $
      evalEmpty
        (SExpr.List
          [ SExpr.Symbol "*"
          , SExpr.Number (ExactInteger 6)
          , SExpr.Number (ExactInteger 7)
          ])
        `shouldBe` Right (Value.Number (ExactInteger 42))

    it "handles division resulting in float: (/ 5 2)" $
      evalEmpty
        (SExpr.List
          [ SExpr.Symbol "/"
          , SExpr.Number (ExactInteger 5)
          , SExpr.Number (ExactInteger 2)
          ])
        `shouldBe` Right (Value.Number (InexactReal 2.5))

    it "division with float operand returns float: (/ 5.0 2)" $
      evalEmpty
        (SExpr.List
          [ SExpr.Symbol "/"
          , SExpr.Number (InexactReal 5.0)
          , SExpr.Number (ExactInteger 2)
          ])
        `shouldBe` Right (Value.Number (InexactReal 2.5))

    it "subtraction preserves exact: (- 10 3)" $
      evalEmpty
        (SExpr.List
          [ SExpr.Symbol "-"
          , SExpr.Number (ExactInteger 10)
          , SExpr.Number (ExactInteger 3)
          ])
        `shouldBe` Right (Value.Number (ExactInteger 7))

  describe "large numbers (R7RS arbitrary precision)" $ do
    it "evaluates very large exact integer" $
      evalEmpty
        (SExpr.Number (ExactInteger 999999999999999999999))
        `shouldBe`
          Right
            (Value.Number
              (ExactInteger 999999999999999999999))

    it "adds large integers without overflow" $
      evalEmpty
        (SExpr.List
          [ SExpr.Symbol "+"
          , SExpr.Number (ExactInteger 999999999999999999)
          , SExpr.Number (ExactInteger 1)
          ])
        `shouldBe`
          Right
            (Value.Number
              (ExactInteger 1000000000000000000))

    it "multiplies large integers" $
      evalEmpty
        (SExpr.List
          [ SExpr.Symbol "*"
          , SExpr.Number (ExactInteger 1000000000)
          , SExpr.Number (ExactInteger 1000000000)
          ])
        `shouldBe`
          Right
            (Value.Number
              (ExactInteger 1000000000000000000))

    it "negates large integer" $
      evalEmpty
        (SExpr.List
          [ SExpr.Symbol "-"
          , SExpr.Number (ExactInteger 999999999999999999)
          ])
        `shouldBe`
          Right
            (Value.Number
              (ExactInteger (-999999999999999999)))

  describe "subnormal and boundary floats" $ do
    it "evaluates very small positive float" $
      evalEmpty (SExpr.Number (InexactReal 1e-100))
        `shouldBe` Right (Value.Number (InexactReal 1e-100))

    it "evaluates very small negative float" $
      evalEmpty (SExpr.Number (InexactReal (-1e-100)))
        `shouldBe` Right (Value.Number (InexactReal (-1e-100)))

    it "adds very small floats" $
      evalEmpty
        (SExpr.List
          [ SExpr.Symbol "+"
          , SExpr.Number (InexactReal 1e-100)
          , SExpr.Number (InexactReal 1e-100)
          ])
        `shouldBe` Right (Value.Number (InexactReal 2e-100))

    it "compares very small floats: (< 1e-100 2e-100)" $
      evalEmpty
        (SExpr.List
          [ SExpr.Symbol "<"
          , SExpr.Number (InexactReal 1e-100)
          , SExpr.Number (InexactReal 2e-100)
          ])
        `shouldBe` Right (Value.Boolean True)

  describe "quote edge cases" $ do
    it "evaluates (quote (quote x)) to quoted symbol" $
      evalEmpty
        (SExpr.List
          [ SExpr.Symbol "quote"
          , SExpr.List
              [ SExpr.Symbol "quote"
              , SExpr.Symbol "x"
              ]
          ])
        `shouldBe`
          Right
            (Value.List
              [ Value.Symbol "quote"
              , Value.Symbol "x"
              ])

    it "evaluates (quote ((+ 1 2))) to quoted nested list" $
      evalEmpty
        (SExpr.List
          [ SExpr.Symbol "quote"
          , SExpr.List
              [ SExpr.List
                  [ SExpr.Symbol "+"
                  , SExpr.Number (ExactInteger 1)
                  , SExpr.Number (ExactInteger 2)
                  ]
              ]
          ])
        `shouldBe`
          Right
            (Value.List
              [ Value.List
                  [ Value.Symbol "+"
                  , Value.Number (ExactInteger 1)
                  , Value.Number (ExactInteger 2)
                  ]
              ])

  describe "if lazy evaluation (R7RS)" $ do
    it "evaluates (if #t 1 ...) without evaluating else branch" $
      evalEmpty
        (SExpr.List
          [ SExpr.Symbol "if"
          , SExpr.Boolean True
          , SExpr.Number (ExactInteger 1)
          , SExpr.List [SExpr.Symbol "undefined-op"]
          ])
        `shouldBe` Right (Value.Number (ExactInteger 1))

    it "evaluates (if #f ... 2) without evaluating then branch" $
      evalEmpty
        (SExpr.List
          [ SExpr.Symbol "if"
          , SExpr.Boolean False
          , SExpr.List [SExpr.Symbol "undefined-op"]
          , SExpr.Number (ExactInteger 2)
          ])
        `shouldBe` Right (Value.Number (ExactInteger 2))

  describe "errors" $ do
    it "fails on an empty list" $
      evalEmpty (SExpr.List [])
        `shouldBe` Left (CannotEvaluate (SExpr.List []))

    it "fails when an operator is unbound" $
      let form =
            SExpr.List
              [ SExpr.Symbol "unknown-op"
              , SExpr.Number (ExactInteger 1)
              ]
      in evalEmpty form
           `shouldBe` Left (UnboundVariable "unknown-op")

    it "fails on division by zero" $
      let form =
            SExpr.List
              [ SExpr.Symbol "/"
              , SExpr.Number (ExactInteger 1)
              , SExpr.Number (ExactInteger 0)
              ]
      in evalEmpty form
           `shouldBe` Left (CannotEvaluate form)
