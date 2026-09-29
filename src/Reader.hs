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

import SExpr (Number(..), SExpr(..))

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


tokenize :: String -> [Token]
tokenize "" = []
tokenize program@(first:rest)
  | isSpace first = tokenize rest
  | first == '('  = LParen : tokenize rest
  | first == ')'  = RParen : tokenize rest
  | otherwise     =
    let (atomText, remainingProgram) = break isTokenDelimiter program
    in Atom atomText : tokenize remainingProgram
  where
    nonSpaceDelimiters :: [Char]
    nonSpaceDelimiters = "()"

    isTokenDelimiter :: Char -> Bool
    isTokenDelimiter char = char `elem` nonSpaceDelimiters || isSpace char


readSExpr :: [Token] -> Either ReaderError (SExpr, [Token])
readSExpr []                  = Left UnexpectedEOF
readSExpr (RParen : _)        = Left UnexpectedRParen
readSExpr (Atom text : rest)  = Right (readAtom text, rest)
readSExpr (LParen : rest)     = do
  (expressions, remainingTokens) <- readListContents rest
  Right (SExpr.List expressions, remainingTokens)

readListContents :: [Token] -> Either ReaderError ([SExpr], [Token])
readListContents []               = Left UnterminatedList
readListContents (RParen : rest)  = Right ([], rest)
readListContents tokens           = do
  (firstExpression, remainingTokens) <- readSExpr tokens
  (restExpressions, finalTokens) <- readListContents remainingTokens
  Right (firstExpression : restExpressions, finalTokens)

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

readInteger :: String -> Maybe SExpr.Number
readInteger text =
  SExpr.ExactInteger <$> readMaybe text

readReal :: String -> Maybe SExpr.Number
readReal text =
  SExpr.InexactReal <$> readMaybe text

readProgram :: String -> Either ReaderError [SExpr]
readProgram program =
  readExpressions (tokenize program)
  where
    readExpressions [] = Right []
    readExpressions tokens = do
      (expression, remainingTokens) <- readSExpr tokens
      remainingExpressions <- readExpressions remainingTokens
      pure (expression : remainingExpressions)


renderTokens :: [Token] -> String
renderTokens = unwords . map renderToken
  where
    renderToken LParen      = "("
    renderToken RParen      = ")"
    renderToken (Atom text) = text

renderReaderError :: ReaderError -> String
renderReaderError UnexpectedEOF     = "Unexpected end of input."
renderReaderError UnexpectedRParen  = "Unexpected ')'."
renderReaderError UnterminatedList  = "Unterminated list; expected ')'."
