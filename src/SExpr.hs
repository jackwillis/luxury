module SExpr
  ( SExpr(..)
  , render
  ) where

data SExpr
  = Symbol String
  | Number Int
  | List [SExpr]
  deriving (Eq, Show)

render :: SExpr -> String
render (Symbol name)  = name
render (Number value) = show value
render (List elems)   = "(" <> unwords (map render elems) <> ")"
