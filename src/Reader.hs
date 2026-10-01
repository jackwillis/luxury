-- source -> SExpr
module Reader
  ( Token(..)
  , ReaderError(..)
  , tokenize
  , renderReaderError
  , readSExpr
  , readProgram
  ) where

import Control.Applicative ((<|>))
import Data.Char (isAsciiLower, isAsciiUpper, isSpace, toLower)
import Data.Maybe (fromMaybe)
import Text.Read (readMaybe)

import Number (Number(..))
import SExpr (SExpr(..))


data Token
  = LParen
  | RParen
  | Quote
  | Atom String
  deriving (Eq, Show)

data ReaderError
  = UnexpectedEOF
  | UnexpectedRParen
  | UnterminatedList
  | InvalidAtom String
  deriving (Eq, Show)

renderToken :: Token -> String
renderToken LParen      = "("
renderToken RParen      = ")"
renderToken Quote       = "'"
renderToken (Atom text) = text

renderTokens :: [Token] -> String
renderTokens = unwords . map renderToken

renderReaderError :: ReaderError -> String
renderReaderError UnexpectedEOF     = "Unexpected end of input."
renderReaderError UnexpectedRParen  = "Unexpected ')'."
renderReaderError UnterminatedList  = "Unterminated list; expected ')'."
renderReaderError (InvalidAtom text) = "Invalid or unsupported atom: " <> text


tokenize :: String -> [Token]
tokenize "" = []
tokenize programText@(first:rest)
  | isSpace first = tokenize rest
  | first == ';'  = tokenize (dropRestOfCurrentLine rest)
  | first == '('  = LParen  : tokenize rest
  | first == ')'  = RParen  : tokenize rest
  | first == '\'' = Quote   : tokenize rest
  | otherwise     =
    let (atomText, remainingProgramText) = break isTokenDelimiter programText
    in Atom atomText : tokenize remainingProgramText
  where
    dropRestOfCurrentLine :: String -> String
    dropRestOfCurrentLine =
      dropWhile (/= '\n')

    isTokenDelimiter :: Char -> Bool
    isTokenDelimiter char =
      char `elem` nonSpaceDelimiters || isSpace char

    nonSpaceDelimiters :: [Char]
    nonSpaceDelimiters = "();'"


-- reads one complete expression from the front of the tokens, returning
-- whatever tokens are left over; leftover tokens are not an error
readSExpr :: [Token] -> Either ReaderError (SExpr, [Token])
-- No tokens means no expression is available to satisfy readSExpr's contract.
readSExpr [] =
  Left UnexpectedEOF

-- a close paren with nothing before it doesn't open anything
readSExpr (RParen : _) =
  Left UnexpectedRParen

-- an atom is a complete expression on its own
readSExpr (Atom text : rest) = do
  expression <- readAtom text
  Right (expression, rest)

-- quote shorthand reads the following datum and expands it to (quote datum)
readSExpr (Quote : rest) = do
  (expression, remainingTokens) <- readSExpr rest
  Right
    ( SExpr.List
        [ SExpr.Symbol "quote"
        , expression
        ]
    , remainingTokens
    )

-- an open paren starts a list; read its contents up to the matching close
readSExpr (LParen : rest) = do
  (expressions, remainingTokens) <- readListContents rest
  Right (SExpr.List expressions, remainingTokens)


-- reads the elements of a list up to (and consuming) its closing paren;
-- the opening paren has already been consumed by the caller
readListContents :: [Token] -> Either ReaderError ([SExpr], [Token])
-- ran out of tokens before finding the closing paren
readListContents [] =
  Left UnterminatedList

-- the closing paren: no more elements, stop here
readListContents (RParen : rest) =
  Right ([], rest)

-- anything else: read one element, then recurse for the rest of the list
readListContents tokens = do
  (firstExpression, remainingTokens) <- readSExpr tokens
  (restExpressions, finalTokens) <- readListContents remainingTokens
  Right (firstExpression : restExpressions, finalTokens)


-- Validate Scheme syntax before converting numeric text with Haskell's reader.
-- Unsupported datum syntax is an error, rather than an arbitrary symbol.
readAtom :: String -> Either ReaderError SExpr
readAtom text =
  case readBoolean text <|> readNumber text <|> readSymbol text of
    Just value -> Right value
    Nothing -> Left (InvalidAtom text)

readSymbol :: String -> Maybe SExpr
readSymbol text
  | validIdentifier text = Just (SExpr.Symbol text)
  | otherwise = Nothing

readBoolean :: String -> Maybe SExpr
readBoolean text =
  SExpr.Boolean <$> case map toLower text of
    "#t"     -> Just True
    "#true"  -> Just True
    "#f"     -> Just False
    "#false" -> Just False
    _        -> Nothing

-- The unescaped ASCII identifier grammar from R7RS section 7.1.1.
-- Escaped identifiers and additional Unicode characters are deferred.
validIdentifier :: String -> Bool
validIdentifier text
  | map toLower text `elem` ["+i", "-i", "+inf.0", "-inf.0", "+nan.0", "-nan.0"] = False
  | otherwise = case text of
      [] -> False
      first : rest
        | initial first -> all subsequent rest
        | first `elem` "+-" -> case rest of
            [] -> True
            '.' : next : remaining -> dotSubsequent next && all subsequent remaining
            next : remaining -> signSubsequent next && all subsequent remaining
        | first == '.' -> case rest of
            next : remaining -> dotSubsequent next && all subsequent remaining
            [] -> False
        | otherwise -> False
  where
    -- Ordinary identifiers start with a letter or Scheme's special initial
    -- punctuation. Digits, signs, dots, and @ cannot start this form.
    initial char = isAsciiLower char || isAsciiUpper char || char `elem` "!$%&*/:<=>?^_~"

    -- After the initial character, digits, signs, dots, and @ are also allowed
    -- (for example, list->vector, a1, and foo.bar).
    subsequent char = initial char || decimalDigit char || char `elem` "+-.@"

    -- Immediately after a leading + or -, an identifier needs an initial
    -- character, another sign, or @. A digit instead belongs to numeric syntax.
    -- A dot takes the separate sign-dot branch above.
    signSubsequent char = initial char || char `elem` "+-@"

    -- Immediately after a leading dot (or sign followed by dot), another dot
    -- is allowed too: ... and +.. are identifiers, while .5 is numeric syntax.
    dotSubsequent char = signSubsequent char || char == '.'

decimalDigit :: Char -> Bool
decimalDigit char = char >= '0' && char <= '9'

readNumber :: String -> Maybe SExpr
readNumber text = do
  let (sign, unsigned) = case text of
        '+' : rest -> ("", rest)
        '-' : rest -> ("-", rest)
        _ -> ("", text)
      (whole, rest) = span decimalDigit unsigned
  case rest of
    [] | not (null whole) ->
      SExpr.Number . ExactInteger <$> readMaybe (sign <> whole)
    _ -> do
      let (mantissa, suffix) = case rest of
            '.' : afterDot ->
              let (fraction, remaining) = span decimalDigit afterDot
              in (fromMaybe "0" (nonempty whole) <> "." <>
                  fromMaybe "0" (nonempty fraction), remaining)
            _ -> (whole <> ".0", rest)
          hasDigits = not (null whole) || case rest of
            '.' : next : _ -> decimalDigit next
            _ -> False
      exponent <- readExponent suffix
      if hasDigits
        then SExpr.Number . InexactReal <$> readMaybe (sign <> mantissa <> exponent)
        else Nothing
  where
    nonempty "" = Nothing
    nonempty value = Just value

    readExponent "" = Just ""
    readExponent (marker : rest)
      | marker `elem` "eE" =
          let (sign, digits) = case rest of
                '+' : remaining -> ("", remaining)
                '-' : remaining -> ("-", remaining)
                _ -> ("", rest)
          in if not (null digits) && all decimalDigit digits
               then Just ("e" <> sign <> digits)
               else Nothing
    readExponent _ = Nothing


-- reads a whole program as a sequence of top-level forms, not a single
-- expression; there is no "trailing input" error here, since it just keeps
-- reading forms until the tokens run out
readProgram :: String -> Either ReaderError [SExpr]
readProgram program =
  readExpressions (tokenize program)
  where
    -- no tokens left: the program ends cleanly between forms
    readExpressions [] = Right []

    -- more tokens: read one top-level form, then recurse for the rest
    readExpressions tokens = do
      (expression, remainingTokens) <- readSExpr tokens
      remainingExpressions <- readExpressions remainingTokens
      pure (expression : remainingExpressions)
