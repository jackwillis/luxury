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
import Data.Char (isSpace)
import Data.Maybe (fromMaybe)
import Text.Read (readMaybe)

import Number (Number(..))
import SExpr (SExpr(..))


data Token
  = LParen
  | RParen
  | Atom String
  deriving (Eq, Show)

data ReaderError
  = UnexpectedEOF
  | UnexpectedRParen
  | UnterminatedList
  deriving (Eq, Show)

renderToken :: Token -> String
renderToken LParen      = "("
renderToken RParen      = ")"
renderToken (Atom text) = text

renderTokens :: [Token] -> String
renderTokens = unwords . map renderToken

renderReaderError :: ReaderError -> String
renderReaderError UnexpectedEOF     = "Unexpected end of input."
renderReaderError UnexpectedRParen  = "Unexpected ')'."
renderReaderError UnterminatedList  = "Unterminated list; expected ')'."


tokenize :: String -> [Token]
tokenize "" = []
tokenize programText@(first:rest)
  | isSpace first = tokenize rest
  | first == '('  = LParen : tokenize rest
  | first == ')'  = RParen : tokenize rest
  | otherwise     =
    let (atomText, remainingProgramText) = break isTokenDelimiter programText
    in Atom atomText : tokenize remainingProgramText
  where
    nonSpaceDelimiters :: [Char]
    nonSpaceDelimiters = "()"

    isTokenDelimiter :: Char -> Bool
    isTokenDelimiter char = char `elem` nonSpaceDelimiters || isSpace char


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
readSExpr (Atom text : rest) =
  Right (readAtom text, rest)

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


-- classifies a single atom's text as a boolean, a number, or (falling back)
-- a symbol; order matters, since a bare "-" or "4.2.3" must fall through to
-- readNumber's failure before landing on Symbol
readAtom :: String -> SExpr
readAtom text =
  fromMaybe (readSymbol text) $
        readBoolean text
    <|> readNumber text

readSymbol :: String -> SExpr
readSymbol name = SExpr.Symbol name

readBoolean :: String -> Maybe SExpr
readBoolean text =
  SExpr.Boolean <$> case text of
    "#t"  -> Just True
    "#T"  -> Just True
    "#f"  -> Just False
    "#F"  -> Just False
    _     -> Nothing

readNumber :: String -> Maybe SExpr
readNumber text =
  SExpr.Number <$> ((readInteger text) <|> (readReal text))

readInteger :: String -> Maybe Number
readInteger text =
  ExactInteger <$> readMaybe text

readReal :: String -> Maybe Number
readReal text =
  InexactReal <$> readMaybe text


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
