module Reader where

import Data.Char (isSpace)
import Data.List (dropWhileEnd, groupBy)

data Token
  = LParen
  | RParen
  | Atom String
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

isTokenDelimiter :: Char -> Bool
isTokenDelimiter char = char == '(' || char == ')' || isSpace char
