-- SExpr -> Value
module Eval
  ( EvalError(..)
  , eval
  ) where

import Number (Number(..))
import SExpr (SExpr(..))
import qualified Value

data EvalError
  = CannotEvaluate SExpr
  deriving (Eq, Show)

eval :: SExpr -> Either EvalError Value.Value
eval (SExpr.Number number) =
  Right (Value.Number number)

eval (SExpr.Boolean boolean) =
  Right (Value.Boolean boolean)

eval (SExpr.List [SExpr.Symbol "quote", expression]) =
  Right (quoteDatum expression)

eval expression =
  Left (CannotEvaluate expression)

quoteDatum :: SExpr -> Value.Value
quoteDatum (SExpr.Number number) =
  Value.Number number

quoteDatum (SExpr.Boolean boolean) =
  Value.Boolean boolean

quoteDatum (SExpr.Symbol name) =
  Value.Symbol name

quoteDatum (SExpr.List elements) =
  Value.List (map quoteDatum elements)
