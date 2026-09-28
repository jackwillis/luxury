module ReaderSpec (spec) where

import Reader
import Test.Hspec

spec :: Spec
spec = describe "Reader.tokenize" $ do
  describe "basics" $ do
    it "tokenizes an empty string as no tokens" $
      tokenize "" `shouldBe` []

    it "tokenizes a single atom" $
      tokenize "foo" `shouldBe` [Atom "foo"]

    it "tokenizes a lone open paren" $
      tokenize "(" `shouldBe` [LParen]

    it "tokenizes a lone close paren" $
      tokenize ")" `shouldBe` [RParen]

    it "tokenizes an empty list" $
      tokenize "()" `shouldBe` [LParen, RParen]

  describe "whitespace handling" $ do
    it "tokenizes whitespace-only input as no tokens" $
      tokenize "   " `shouldBe` []

    it "trims leading and trailing whitespace around an atom" $
      tokenize "  foo  " `shouldBe` [Atom "foo"]

    it "treats tabs and newlines as separators, same as spaces" $
      tokenize "foo\tbar\n" `shouldBe` [Atom "foo", Atom "bar"]

  describe "multiple atoms and real expressions" $ do
    it "tokenizes a flat expression" $
      tokenize "(+ 2 3)" `shouldBe` [LParen, Atom "+", Atom "2", Atom "3", RParen]

    it "tokenizes nested lists" $
      tokenize "(a (b c))"
        `shouldBe` [LParen, Atom "a", LParen, Atom "b", Atom "c", RParen, RParen]

    it "splits adjacent parens into separate tokens even with no whitespace between them" $
      tokenize "(())" `shouldBe` [LParen, LParen, RParen, RParen]

    it "tokenizes multiple top-level atoms with no wrapping list" $
      tokenize "foo bar" `shouldBe` [Atom "foo", Atom "bar"]

  describe "atom character variety" $ do
    it "allows ! in an atom" $
      tokenize "set!" `shouldBe` [Atom "set!"]

    it "allows -> in an atom" $
      tokenize "list->vector" `shouldBe` [Atom "list->vector"]

    it "treats a leading - followed by digits as one atom, not a separate symbol" $
      tokenize "-5" `shouldBe` [Atom "-5"]

    it "allows a decimal point in an atom" $
      tokenize "3.14" `shouldBe` [Atom "3.14"]
