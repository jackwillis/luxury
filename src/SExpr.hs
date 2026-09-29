-- reader-level representation
module SExpr
  ( SExpr(..)
  , Number(..)
  , render
  ) where

import Number (Number(..))
import qualified Number

data SExpr
  = Symbol String
  | Number Number
  | Boolean Bool
  | List [SExpr]
  deriving (Eq, Show)

render :: SExpr -> String

render (Symbol name) =
  name

render (Number number) =
  Number.render number

render (Boolean valence) =
  case valence of
    True  -> "#t"
    False -> "#f"

render (List elements) =
  "(" <> unwords (map render elements) <> ")"
