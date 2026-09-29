-- SExpr -> Value
module Eval
  ( EvalError(..)
  , eval
  , renderEvalError
  ) where

import Env (Env)
import qualified Env
import SExpr (SExpr(..))
import qualified SExpr
import Value (Value)
import qualified Value


data EvalError
  = UnboundVariable String
  | CannotEvaluate SExpr
  deriving (Eq, Show)

renderEvalError :: EvalError -> String
renderEvalError (UnboundVariable name) =
  "Unbound variable: " <> name

renderEvalError (CannotEvaluate expression) =
  "Cannot evaluate: " <> SExpr.render expression


eval :: Env -> SExpr -> Either EvalError Value
eval env (SExpr.Symbol name) =
  case Env.lookup name env of
    Just value ->
      Right value

    Nothing ->
      Left (UnboundVariable name)

eval _env (SExpr.Number number) =
  Right (Value.Number number)

eval _env (SExpr.Boolean boolean) =
  Right (Value.Boolean boolean)

eval _env (SExpr.List [SExpr.Symbol "quote", expression]) =
  Right (quoteDatum expression)

eval _env expression =
  Left (CannotEvaluate expression)


quoteDatum :: SExpr -> Value
quoteDatum (SExpr.Number number) =
  Value.Number number

quoteDatum (SExpr.Boolean boolean) =
  Value.Boolean boolean

quoteDatum (SExpr.Symbol name) =
  Value.Symbol name

quoteDatum (SExpr.List elements) =
  Value.List (map quoteDatum elements)
