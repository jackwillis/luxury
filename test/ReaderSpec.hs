module ReaderSpec (spec) where

import Reader
import SExpr
import Test.Hspec

spec :: Spec
spec = do
  tokenizeSpec
  renderReaderErrorSpec
  readSExprSpec

tokenizeSpec :: Spec
tokenizeSpec = describe "Reader.tokenize" $ do
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

renderReaderErrorSpec :: Spec
renderReaderErrorSpec = describe "Reader.renderReaderError" $ do
  it "renders UnexpectedEOF" $
    renderReaderError UnexpectedEOF `shouldBe` "Unexpected end of input."

  it "renders UnexpectedRParen" $
    renderReaderError UnexpectedRParen `shouldBe` "Unexpected ')'."

  it "renders UnterminatedList" $
    renderReaderError UnterminatedList `shouldBe` "Unterminated list; expected ')'."

  it "renders TrailingTokens, showing the leftover tokens" $
    renderReaderError (TrailingTokens [Atom "bar"])
      `shouldBe` "Unexpected input after expression: bar."

readSExprSpec :: Spec
readSExprSpec = describe "Reader.readSExpr" $ do
  describe "atoms" $ do
    it "reads a non-numeric atom as a Symbol" $
      readSExpr [Atom "foo"] `shouldBe` Right (Symbol "foo", [])

    it "reads a numeric atom as a Number" $
      readSExpr [Atom "42"] `shouldBe` Right (Number 42, [])

    it "reads a negative numeric atom as a Number" $
      readSExpr [Atom "-5"] `shouldBe` Right (Number (-5), [])

  describe "lists" $ do
    it "reads an empty list" $
      readSExpr [LParen, RParen] `shouldBe` Right (List [], [])

    it "reads a flat list" $
      readSExpr [LParen, Atom "+", Atom "2", Atom "3", RParen]
        `shouldBe` Right (List [Symbol "+", Number 2, Number 3], [])

    it "reads nested lists" $
      readSExpr [LParen, Atom "a", LParen, Atom "b", Atom "c", RParen, RParen]
        `shouldBe` Right (List [Symbol "a", List [Symbol "b", Symbol "c"]], [])

  describe "leftover tokens" $ do
    it "returns tokens after a single atom as leftovers, not an error" $
      readSExpr [Atom "foo", Atom "bar"] `shouldBe` Right (Symbol "foo", [Atom "bar"])

    it "returns tokens after a complete list as leftovers, not an error" $
      readSExpr [LParen, Atom "+", Atom "2", Atom "3", RParen, Atom "extra"]
        `shouldBe` Right (List [Symbol "+", Number 2, Number 3], [Atom "extra"])

  describe "errors" $ do
    it "fails on no tokens at all" $
      readSExpr [] `shouldBe` Left UnexpectedEOF

    it "fails on a leading close paren" $
      readSExpr [RParen] `shouldBe` Left UnexpectedRParen

    it "fails on an unterminated empty list" $
      readSExpr [LParen] `shouldBe` Left UnterminatedList

    it "fails on an unterminated list with contents" $
      readSExpr [LParen, Atom "a"] `shouldBe` Left UnterminatedList
