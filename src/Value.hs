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
  deriving (Eq, Show)

render :: Value -> String
render (Number number) = Number.render number
render (Boolean True)  = "#t"
render (Boolean False) = "#f"
