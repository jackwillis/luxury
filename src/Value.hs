-- runtime representation
module Value
  ( Primitive(..)
  , Value(..)
  , render
  ) where

import Number (Number)
import qualified Number


data Value
  = Number Number
  | Boolean Bool
  | Symbol String
  | EmptyList
  | Pair Value Value
  | PrimitiveProcedure Primitive
  deriving (Eq, Show)

data Primitive
  = Cons
  | Car
  | Cdr
  | PairP
  | NullP
  | EqP
  | EqvP
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

render EmptyList =
  "()"

render pair@(Pair _ _) =
  "(" <> renderPair pair <> ")"
  where
    renderPair :: Value -> String
    renderPair (Pair first EmptyList) =
      render first

    renderPair (Pair first rest@(Pair _ _)) =
      render first <> " " <> renderPair rest

    renderPair (Pair first rest) =
      render first <> " . " <> render rest

    renderPair value =
      render value

render (PrimitiveProcedure primitive) =
  "#<primitive:" <> renderPrimitive primitive <> ">"
  where
    renderPrimitive primitive =
      case primitive of
        Cons  -> "cons"
        Car   -> "car"
        Cdr   -> "cdr"
        PairP -> "pair?"
        NullP -> "null?"
        EqP   -> "eq?"
        EqvP  -> "eqv?"