-- SExpr -> Value
module Eval
  ( EvalError(..)
  , eval
  ) where

import SExpr (SExpr(..), Number(..))
import qualified Value

data EvalError
  = CannotEvaluate SExpr
  deriving (Eq, Show)

eval :: SExpr -> Either EvalError Value.Value
eval (SExpr.Number number) =
  Right (Value.Number number)

eval (SExpr.Boolean boolean) =
  Right (Value.Boolean boolean)

eval expression =
  Left (CannotEvaluate expression)
