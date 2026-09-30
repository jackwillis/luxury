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
  atomBoundarySpec

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

  describe "comments" $ do
    it "ignores a comment-only line" $
      tokenize "; hello" `shouldBe` []

    it "ignores a double-semicolon comment" $
      tokenize ";; hello" `shouldBe` []

    it "ignores a comment after an expression" $
      tokenize "foo ; hello"
        `shouldBe` [Atom "foo"]

    it "allows comments without whitespace before them" $
      tokenize "foo; hello"
        `shouldBe` [Atom "foo"]

    it "continues tokenizing after the newline" $
      tokenize "foo ; comment\nbar"
        `shouldBe` [Atom "foo", Atom "bar"]

    it "ignores comments inside a list" $
      tokenize "(a ; comment\n b)"
        `shouldBe` [LParen, Atom "a", Atom "b", RParen]

  describe "quote shorthand" $ do
    it "tokenizes 'foo as quote and atom" $
      tokenize "'foo" `shouldBe` [Quote, Atom "foo"]

    it "tokenizes '(a b) as quote and list" $
      tokenize "'(a b)" `shouldBe` [Quote, LParen, Atom "a", Atom "b", RParen]

    it "treats quote as a delimiter in foo'bar" $
      tokenize "foo'bar" `shouldBe` [Atom "foo", Quote, Atom "bar"]

    it "tokenizes nested quotes ''foo" $
      tokenize "''foo" `shouldBe` [Quote, Quote, Atom "foo"]

    it "tokenizes multiple quoted forms" $
      tokenize "'a 'b" `shouldBe` [Quote, Atom "a", Quote, Atom "b"]

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

    it "rejects multiple decimal points" $
      readSExpr [Atom "4.2.3"] `shouldBe` Left (InvalidAtom "4.2.3")

  describe "booleans" $ do
    it "reads #t as True" $
      readSExpr [Atom "#t"] `shouldBe` Right (Boolean True, [])

    it "reads #T as True" $
      readSExpr [Atom "#T"] `shouldBe` Right (Boolean True, [])

    it "reads #f as False" $
      readSExpr [Atom "#f"] `shouldBe` Right (Boolean False, [])

    it "reads #F as False" $
      readSExpr [Atom "#F"] `shouldBe` Right (Boolean False, [])

    it "rejects an unknown hash-prefixed atom" $
      readSExpr [Atom "#nope"] `shouldBe` Left (InvalidAtom "#nope")

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
      readSExpr [Atom ".5"] `shouldBe` Right (Number (InexactReal 0.5), [])

    it "reads a float with a trailing decimal point" $
      readSExpr [Atom "3."] `shouldBe` Right (Number (InexactReal 3.0), [])

    it "reads leading zero integer" $
      readSExpr [Atom "007"] `shouldBe` Right (Number (ExactInteger 7), [])

    it "reads leading zeros for float" $
      readSExpr [Atom "00.5"] `shouldBe` Right (Number (InexactReal 0.5), [])

    it "reads negative zero" $
      readSExpr [Atom "-0"] `shouldBe` Right (Number (ExactInteger 0), [])

    it "reads plus sign as a symbol" $
      readSExpr [Atom "+"] `shouldBe` Right (Symbol "+", [])

    it "reads an explicitly positive integer" $
      readSExpr [Atom "+5"] `shouldBe` Right (Number (ExactInteger 5), [])

    it "reads double negative as symbol" $
      readSExpr [Atom "--5"] `shouldBe` Right (Symbol "--5", [])

    it "reads number with spaces as multiple tokens" $
      tokenize "1 2 3" `shouldBe` [Atom "1", Atom "2", Atom "3"]

  describe "R7RS atom classification" $ do
    mapM_ (\text -> it ("reads boolean " <> text) $
      readProgram text `shouldBe` Right [Boolean True])
      ["#true", "#TRUE", "#TrUe"]
    mapM_ (\text -> it ("reads boolean " <> text) $
      readProgram text `shouldBe` Right [Boolean False])
      ["#false", "#FALSE", "#FaLsE"]
    mapM_ (\(text, value) -> it ("reads decimal " <> text) $
      readProgram text `shouldBe` Right [Number (InexactReal value)])
      [("+.5", 0.5), ("-.5", -0.5), ("1e3", 1000),
       ("1E-2", 0.01), ("3.e+2", 300), (".5e1", 5)]
    mapM_ (\text -> it ("preserves identifier " <> text) $
      readProgram text `shouldBe` Right [Symbol text])
      ["...", "+foo", "-.foo", ".foo", "--5", "foo.bar", "set!"]
    mapM_ (\text -> it ("rejects malformed or unsupported atom " <> text) $
      readProgram text `shouldBe` Left (InvalidAtom text))
      ["1e", "1e+", "1e2e3", "123abc", "#truex", "4.2.3", ".", "@foo",
       "#xFF", "1/2", "+i", "+inf.0"]
    it "propagates invalid atoms inside lists" $
      readProgram "(foo 1e+)" `shouldBe` Left (InvalidAtom "1e+")
    it "renders an invalid atom" $
      renderReaderError (InvalidAtom "#nope")
        `shouldBe` "Invalid or unsupported atom: #nope"

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

  describe "quote shorthand" $ do
    it "reads [Quote, Atom \"foo\"] as (quote foo)" $
      readSExpr [Quote, Atom "foo"]
        `shouldBe` Right (List [Symbol "quote", Symbol "foo"], [])

    it "reads [Quote, LParen, ...] as (quote (...))" $
      readSExpr [Quote, LParen, Atom "a", Atom "b", RParen]
        `shouldBe` Right (List [Symbol "quote", List [Symbol "a", Symbol "b"]], [])

    it "reads nested quotes as (quote (quote foo))" $
      readSExpr [Quote, Quote, Atom "foo"]
        `shouldBe` Right (List [Symbol "quote", List [Symbol "quote", Symbol "foo"]], [])

    it "fails on dangling Quote token" $
      readSExpr [Quote]
        `shouldBe` Left UnexpectedEOF

    it "preserves leftover tokens after quote" $
      readSExpr [Quote, Atom "foo", Atom "bar"]
        `shouldBe` Right (List [Symbol "quote", Symbol "foo"], [Atom "bar"])

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

  describe "comments" $ do
    it "ignores comments between forms" $
      readProgram
        "foo\n;; => foo\nbar\n;; => bar"
        `shouldBe`
          Right
            [ Symbol "foo"
            , Symbol "bar"
            ]

  describe "quote shorthand" $ do
    it "reads 'foo as (quote foo)" $
      readProgram "'foo"
        `shouldBe` Right [List [Symbol "quote", Symbol "foo"]]

    it "reads '(a b) as (quote (a b))" $
      readProgram "'(a b)"
        `shouldBe` Right [List [Symbol "quote", List [Symbol "a", Symbol "b"]]]

    it "reads multiple quoted forms independently" $
      readProgram "'a 'b"
        `shouldBe` Right
          [ List [Symbol "quote", Symbol "a"]
          , List [Symbol "quote", Symbol "b"]
          ]

    it "reads quoted list followed by normal form" $
      readProgram "'(x) y"
        `shouldBe` Right
          [ List [Symbol "quote", List [Symbol "x"]]
          , Symbol "y"
          ]

  describe "errors" $ do
    it "fails on a stray close paren after a valid form" $
      readProgram "foo )" `shouldBe` Left UnexpectedRParen

    it "fails on an unterminated list" $
      readProgram "(foo" `shouldBe` Left UnterminatedList

-- Exercise the public reader with independently specified syntax examples.
atomBoundarySpec :: Spec
atomBoundarySpec = describe "Reader atom boundaries" $ do
  describe "decimal values and exactness" $ do
    -- Bounded generated domain: every integer from -100 through 100.
    -- A leading plus must preserve its exact value, including at zero.
    it "preserves exact signed integers across -100 through 100" $
      mapM_ (\value ->
        let text = if value >= 0 then "+" <> show value else show value
        in readProgram text `shouldBe` Right [Number (ExactInteger value)])
        [-100 .. 100]
    mapM_ (\(text, expected) -> it ("reads " <> text <> " with the right numeric type") $
      readProgram text `shouldBe` Right [Number expected])
      [ ("+0007", ExactInteger 7)
      , ("-0007", ExactInteger (-7))
      , ("+0", ExactInteger 0)
      , ("123456789012345678901234567890", ExactInteger 123456789012345678901234567890)
      , ("+3.", InexactReal 3)
      , ("-3.", InexactReal (-3))
      , ("0e0", InexactReal 0)
      , ("1e0", InexactReal 1)
      , ("-2E+3", InexactReal (-2000))
      , ("+.25E-1", InexactReal 0.025)
      , ("0002.50e02", InexactReal 250)
      , ("1e-100", InexactReal 1e-100)
      ]
    it "preserves the sign of inexact negative zero" $
      case readProgram "-0.0" of
        Right [Number (InexactReal value)] -> isNegativeZero value `shouldBe` True
        result -> expectationFailure ("Expected inexact negative zero, got " <> show result)

  describe "identifiers near numeric syntax" $ do
    mapM_ (\text -> it ("keeps " <> text <> " as a case-sensitive symbol") $
      readProgram text `shouldBe` Right [Symbol text])
      ["Foo", "foo", "e10", "NaN", "Infinity", "+", "-", "++", "-+",
       "+@name", "-@name", ".@name", "+..", "-..", ".+", ".-", "..",
       "a1", "a+b-c.d@e", "list->vector"]
    mapM_ (\char -> it ("allows initial punctuation " <> [char]) $
      readProgram [char] `shouldBe` Right [Symbol [char]])
      "!$%&*/:<=>?^_~"

  describe "malformed decimal syntax" $ do
    mapM_ (\text -> it ("rejects " <> text <> " without accepting a numeric prefix") $
      readProgram text `shouldBe` Left (InvalidAtom text))
      ["1e-", "1e++2", "1e--2", "1e2.0", "1.2.3", ".5foo", "+5foo",
       "1_000", "1s2", "0x10", "#tfoo", "#falsehood", "#TRUE!",
       "[foo]", "foo,bar", "foo\\bar"]
    it "rejects an empty manually constructed atom" $
      readSExpr [Atom ""] `shouldBe` Left (InvalidAtom "")

  describe "composition and leftovers" $ do
    it "does not consume the next atom when reading one decimal" $
      readSExpr [Atom "+.5", Atom "#TRUE"]
        `shouldBe` Right (Number (InexactReal 0.5), [Atom "#TRUE"])
    it "does not validate leftover tokens when asked for one form" $
      readSExpr [Atom "1", Atom "#oops"]
        `shouldBe` Right (Number (ExactInteger 1), [Atom "#oops"])
    it "does validate later atoms when reading a whole program" $
      readProgram "1 #oops" `shouldBe` Left (InvalidAtom "#oops")
    it "propagates an invalid atom through quote" $
      readProgram "'1e+" `shouldBe` Left (InvalidAtom "1e+")
    it "propagates an invalid atom through nested lists" $
      readProgram "(ok (nested #oops))" `shouldBe` Left (InvalidAtom "#oops")
    it "requires a delimiter between a boolean and a number" $
      readProgram "(#TRUE+.5)" `shouldBe` Left (InvalidAtom "#TRUE+.5")
    it "recognizes actual delimiters around booleans and numbers" $
      readProgram "(#TRUE +.5)'#false; comment\n3."
        `shouldBe` Right
          [ List [Boolean True, Number (InexactReal 0.5)]
          , List [Symbol "quote", Boolean False]
          , Number (InexactReal 3)
          ]
