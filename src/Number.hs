-- Scheme numeric domain
module Number
  ( Number(..)
  , render
  ) where

data Number
  = ExactInteger Integer
  | InexactReal Double
  deriving (Eq, Show)

render :: Number -> String
render (ExactInteger quantity) = show quantity
render (InexactReal quantity)  = show quantity
