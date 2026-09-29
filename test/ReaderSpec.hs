module ReaderSpec (spec) where

import Reader
import Number (Number(..))
import SExpr
import Test.Hspec

spec :: Spec
spec = do
  tokenizeSpec
  renderReaderErrorSpec
  readSExprSpec
  readProgramSpec

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

readSExprSpec :: Spec
readSExprSpec = describe "Reader.readSExpr" $ do
  describe "atoms" $ do
    it "reads a non-numeric atom as a Symbol" $
      readSExpr [Atom "foo"] `shouldBe` Right (Symbol "foo", [])

    it "reads a integer atom as a Number" $
      readSExpr [Atom "42"] `shouldBe` Right (Number (ExactInteger 42), [])

    it "reads a negative integer atom as a Number" $
      readSExpr [Atom "-5"] `shouldBe` Right (Number (ExactInteger (-5)), [])

    it "reads a floating atom as a Number" $
      readSExpr [Atom "4.2"] `shouldBe` Right (Number (InexactReal 4.2), [])

    it "reads a negative floating atom as a Number" $
      readSExpr [Atom "-3.3"] `shouldBe` Right (Number (InexactReal (-3.3)), [])

    it "falls back to Symbol for a malformed number (bare minus sign)" $
      readSExpr [Atom "-"] `shouldBe` Right (Symbol "-", [])

    it "falls back to Symbol for a malformed number (multiple decimal points)" $
      readSExpr [Atom "4.2.3"] `shouldBe` Right (Symbol "4.2.3", [])

  describe "booleans" $ do
    it "reads #t as True" $
      readSExpr [Atom "#t"] `shouldBe` Right (Boolean True, [])

    it "reads #T as True" $
      readSExpr [Atom "#T"] `shouldBe` Right (Boolean True, [])

    it "reads #f as False" $
      readSExpr [Atom "#f"] `shouldBe` Right (Boolean False, [])

    it "reads #F as False" $
      readSExpr [Atom "#F"] `shouldBe` Right (Boolean False, [])

    it "falls back to Symbol for a non-boolean # atom" $
      readSExpr [Atom "#nope"] `shouldBe` Right (Symbol "#nope", [])

  describe "number edge cases (R7RS subset)" $ do
    it "reads zero" $
      readSExpr [Atom "0"] `shouldBe` Right (Number (ExactInteger 0), [])

    it "reads zero as float" $
      readSExpr [Atom "0.0"] `shouldBe` Right (Number (InexactReal 0.0), [])

    it "reads large positive integer" $
      readSExpr [Atom "999999999999"] `shouldBe` Right (Number (ExactInteger 999999999999), [])

    it "reads large negative integer" $
      readSExpr [Atom "-999999999999"] `shouldBe` Right (Number (ExactInteger (-999999999999)), [])

    it "reads very small positive float" $
      readSExpr [Atom "0.0001"] `shouldBe` Right (Number (InexactReal 0.0001), [])

    it "reads very small negative float" $
      readSExpr [Atom "-0.0001"] `shouldBe` Right (Number (InexactReal (-0.0001)), [])

    it "reads float without leading digit" $
      readSExpr [Atom ".5"] `shouldBe` Right (Symbol ".5", [])

    it "reads float with trailing decimal as symbol" $
      readSExpr [Atom "3."] `shouldBe` Right (Symbol "3.", [])

    it "reads leading zero integer" $
      readSExpr [Atom "007"] `shouldBe` Right (Number (ExactInteger 7), [])

    it "reads leading zeros for float" $
      readSExpr [Atom "00.5"] `shouldBe` Right (Number (InexactReal 0.5), [])

    it "reads negative zero" $
      readSExpr [Atom "-0"] `shouldBe` Right (Number (ExactInteger 0), [])

    it "reads plus sign as symbol (not implemented)" $
      readSExpr [Atom "+"] `shouldBe` Right (Symbol "+", [])

    it "reads plus prefix number as symbol (not standard R7RS parsing)" $
      readSExpr [Atom "+5"] `shouldBe` Right (Symbol "+5", [])

    it "reads double negative as symbol" $
      readSExpr [Atom "--5"] `shouldBe` Right (Symbol "--5", [])

    it "reads number with spaces as multiple tokens" $
      tokenize "1 2 3" `shouldBe` [Atom "1", Atom "2", Atom "3"]

  describe "lists" $ do
    it "reads an empty list" $
      readSExpr [LParen, RParen] `shouldBe` Right (List [], [])

    it "reads a flat list" $
      readSExpr [LParen, Atom "+", Atom "2", Atom "3", RParen]
        `shouldBe` Right (List [Symbol "+", Number (ExactInteger 2), Number (ExactInteger 3)], [])

    it "reads nested lists" $
      readSExpr [LParen, Atom "a", LParen, Atom "b", Atom "c", RParen, RParen]
        `shouldBe` Right (List [Symbol "a", List [Symbol "b", Symbol "c"]], [])

    it "reads a list containing a boolean" $
      readSExpr [LParen, Atom "not", Atom "#f", RParen]
        `shouldBe` Right (List [Symbol "not", Boolean False], [])

  describe "leftover tokens" $ do
    it "returns tokens after a single atom as leftovers, not an error" $
      readSExpr [Atom "foo", Atom "bar"] `shouldBe` Right (Symbol "foo", [Atom "bar"])

    it "returns tokens after a complete list as leftovers, not an error" $
      readSExpr [LParen, Atom "+", Atom "2", Atom "3", RParen, Atom "extra"]
        `shouldBe` Right (List [Symbol "+", Number (ExactInteger 2), Number (ExactInteger 3)], [Atom "extra"])

  describe "errors" $ do
    it "fails on no tokens at all" $
      readSExpr [] `shouldBe` Left UnexpectedEOF

    it "fails on a leading close paren" $
      readSExpr [RParen] `shouldBe` Left UnexpectedRParen

    it "fails on an unterminated empty list" $
      readSExpr [LParen] `shouldBe` Left UnterminatedList

    it "fails on an unterminated list with contents" $
      readSExpr [LParen, Atom "a"] `shouldBe` Left UnterminatedList

    it "fails on an unterminated list nested inside a terminated one" $
      readSExpr [LParen, Atom "a", LParen, Atom "b"] `shouldBe` Left UnterminatedList

readProgramSpec :: Spec
readProgramSpec = describe "Reader.readProgram" $ do
  describe "empty input" $ do
    it "reads an empty program as no forms" $
      readProgram "" `shouldBe` Right []

    it "reads whitespace-only input as no forms" $
      readProgram "   " `shouldBe` Right []

  describe "multiple top-level forms" $ do
    it "reads a single atom as a one-form program" $
      readProgram "foo" `shouldBe` Right [Symbol "foo"]

    it "reads multiple top-level atoms as multiple forms, not an error" $
      readProgram "foo bar" `shouldBe` Right [Symbol "foo", Symbol "bar"]

    it "reads multiple top-level lists" $
      readProgram "(+ 1 2) (* 3 4)"
        `shouldBe` Right
          [ List [Symbol "+", Number (ExactInteger 1), Number (ExactInteger 2)]
          , List [Symbol "*", Number (ExactInteger 3), Number (ExactInteger 4)]
          ]

    it "reads a mix of lists and atoms" $
      readProgram "(def foo 2) foo"
        `shouldBe` Right
          [ List [Symbol "def", Symbol "foo", Number (ExactInteger 2)]
          , Symbol "foo"
          ]

    it "reads a mix of floats and booleans across forms" $
      readProgram "1.5 #t (not #f)"
        `shouldBe` Right
          [ Number (InexactReal 1.5)
          , Boolean True
          , List [Symbol "not", Boolean False]
          ]

  describe "errors" $ do
    it "fails on a stray close paren after a valid form" $
      readProgram "foo )" `shouldBe` Left UnexpectedRParen

    it "fails on an unterminated list" $
      readProgram "(foo" `shouldBe` Left UnterminatedList
