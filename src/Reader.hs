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

-- Recognizes unescaped ASCII identifiers (R7RS section 7.1.1), including names
-- starting with signs or dots. Numeric forms are tried before identifiers;
-- unsupported numeric exceptions must not silently become symbols.
-- Escaped and Unicode identifiers are deferred.
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
    initial char = isAsciiLower char || isAsciiUpper char || char `elem` "!$%&*/:<=>?^_~"
    subsequent char = initial char || decimalDigit char || char `elem` "+-.@"

    -- After a leading sign, digits would indicate a number.
    signSubsequent char = initial char || char `elem` "+-@"

    -- After a leading dot or sign-dot, another dot is allowed.
    dotSubsequent char = signSubsequent char || char == '.'

decimalDigit :: Char -> Bool
decimalDigit char = char >= '0' && char <= '9'

-- Reads signed decimal integers and reals, including exponent notation.
-- Validates Scheme syntax and normalizes it before using Haskell's reader;
-- malformed or unsupported numeric forms return Nothing.
readNumber :: String -> Maybe SExpr
readNumber text =
  let (sign, unsigned) = readSign text
      (whole, rest) = span decimalDigit unsigned
  in case rest of
    [] | not (null whole) ->
      SExpr.Number . ExactInteger <$> readMaybe (sign <> whole)
    _ -> readDecimal sign whole rest
  where
    -- Builds an inexact real from the sign, whole digits, and remaining text.
    -- Fills missing digits around a decimal point, validates any exponent,
    -- and requires at least one mantissa digit before converting the result.
    readDecimal :: String -> String -> String -> Maybe SExpr
    readDecimal sign whole rest = do
      let (mantissa, suffix) = case rest of
            -- A decimal point: normalize .5 to 0.5 and 3. to 3.0,
            -- leaving any exponent text for readExponent.
            '.' : afterDot ->
              let (fraction, remaining) = span decimalDigit afterDot
              in (defaultIfEmpty "0" whole <> "." <>
                  defaultIfEmpty "0" fraction, remaining)
            -- No decimal point: give an exponent form such as 3e2
            -- a Haskell-readable real mantissa (3.0).
            _ -> (whole <> ".0", rest)
          hasDigits = not (null whole) || case rest of
            -- With no whole digits, a digit after the dot makes .5 valid.
            '.' : next : _ -> decimalDigit next
            -- Otherwise there are no digits; reject forms such as . or .e2.
            _ -> False
      exponent <- readExponent suffix
      if hasDigits
        then
          let number = sign <> mantissa <> exponent
          in SExpr.Number . InexactReal <$> readMaybe number
        else Nothing

    -- Strip a leading sign, retaining only minus for Haskell's numeric reader.
    readSign :: String -> (String, String)
    readSign ('+' : rest) = ("", rest)
    readSign ('-' : rest) = ("-", rest)
    readSign text = ("", text)

    defaultIfEmpty :: String -> String -> String
    defaultIfEmpty fallback "" = fallback
    defaultIfEmpty _ text = text

    readExponent :: String -> Maybe String
    readExponent "" = Just ""
    readExponent (marker : rest)
      | marker `elem` "eE" =
          let (sign, digits) = readSign rest
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
