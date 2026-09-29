module SExpr
  ( SExpr(..)
  , Number(..)
  , render
  ) where

data Number
  = ExactInteger Integer
  | InexactReal Double
  deriving (Eq, Show)

data SExpr
  = Symbol String
  | Number Number
  | Boolean Bool
  | List [SExpr]
  deriving (Eq, Show)

render :: SExpr -> String

render (Symbol name) =
  name

render (Number (ExactInteger quantity)) =
  show quantity

render (Number (InexactReal quantity)) =
  show quantity

render (Boolean valence) =
  case valence of
    True  -> "#t"
    False -> "#f"

render (List elements) =
  "(" <> unwords (map render elements) <> ")"
