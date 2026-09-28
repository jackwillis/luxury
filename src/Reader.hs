module Reader
  ( Token(..)
  , ReaderError(..)
  , tokenize
  , renderReaderError
  , readSExpr
  , readProgram
  ) where

import Data.Char (isSpace)

import SExpr

data Token
  = LParen
  | RParen
  | Atom String
  deriving (Eq, Show)

data ReaderError
  = UnexpectedEOF
  | UnexpectedRParen
  | UnterminatedList
  | TrailingTokens [Token]
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
    isTokenDelimiter :: Char -> Bool
    isTokenDelimiter char = char == '(' || char == ')' || isSpace char


renderTokens :: [Token] -> String
renderTokens = unwords . map renderToken
  where
    renderToken LParen      = "("
    renderToken RParen      = ")"
    renderToken (Atom text) = text


renderReaderError :: ReaderError -> String
renderReaderError UnexpectedEOF =
  "Unexpected end of input."

renderReaderError UnexpectedRParen =
  "Unexpected ')'."

renderReaderError UnterminatedList =
  "Unterminated list; expected ')'."

renderReaderError (TrailingTokens tokens) =
  "Unexpected input after expression: " <> renderTokens tokens <> "."


readSExpr :: [Token] -> Either ReaderError (SExpr.SExpr, [Token])
readSExpr _tokens = Left UnexpectedEOF


readProgram :: String -> Either ReaderError [SExpr]
readProgram _program = Left UnexpectedEOF
