module ValueSpec (spec) where

import Number (Number(..))
import Test.Hspec
import Value

spec :: Spec
spec = describe "Value.render" $ do
  it "renders an exact integer" $
    render (Value.Number (ExactInteger 42)) `shouldBe` "42"

  it "renders a negative exact integer" $
    render (Value.Number (ExactInteger (-7))) `shouldBe` "-7"

  it "renders an inexact real" $
    render (Value.Number (InexactReal 4.2)) `shouldBe` "4.2"

  it "renders #t for True" $
    render (Boolean True) `shouldBe` "#t"

  it "renders #f for False" $
    render (Boolean False) `shouldBe` "#f"

  describe "symbols" $ do
    it "renders a symbol" $
      render (Symbol "foo") `shouldBe` "foo"

    it "renders a symbol with special characters" $
      render (Symbol "list->vector") `shouldBe` "list->vector"

  describe "lists" $ do
    it "renders an empty list" $
      render EmptyList `shouldBe` "()"

    it "renders a pair" $
      render (Pair (Number (ExactInteger 1)) EmptyList) `shouldBe` "(1)"

    it "renders a flat list" $
      render (Pair (Number (ExactInteger 1)) (Pair (Number (ExactInteger 2)) EmptyList)) `shouldBe` "(1 2)"

    it "renders a nested list" $
      render (Pair (Symbol "a") (Pair (Pair (Symbol "b") EmptyList) EmptyList)) `shouldBe` "(a (b))"

    it "renders an improper list" $
      render (Pair (Number (ExactInteger 1)) (Symbol "rest")) `shouldBe` "(1 . rest)"

  describe "primitive procedures" $ do
    it "renders cons primitive" $
      render (PrimitiveProcedure Cons) `shouldBe` "#<primitive:cons>"

    it "renders car primitive" $
      render (PrimitiveProcedure Car) `shouldBe` "#<primitive:car>"

    it "renders cdr primitive" $
      render (PrimitiveProcedure Cdr) `shouldBe` "#<primitive:cdr>"

    it "renders pair? primitive" $
      render (PrimitiveProcedure PairP) `shouldBe` "#<primitive:pair?>"

    it "renders null? primitive" $
      render (PrimitiveProcedure NullP) `shouldBe` "#<primitive:null?>"
