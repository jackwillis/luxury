-- runtime representation
module Value
  ( Value(..)
  , render
  ) where

import Number (Number)
import qualified Number


data Value
  = Number Number
  | Boolean Bool
  | Symbol String
  | List [Value]
  deriving (Eq, Show)

render :: Value -> String
render (Number number) =
  Number.render number

render (Boolean valence) =
  case valence of
    True  -> "#t"
    False -> "#f"

render (Symbol name) =
  name

render (List elements) =
  "(" <> unwords (map render elements) <> ")"
