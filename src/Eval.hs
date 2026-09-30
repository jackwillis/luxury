-- SExpr -> Value
module Eval
  ( EvalError(..)
  , apply
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
  | NotProcedure Value
  | WrongArity Value.Primitive Int Int
  | ExpectedPair Value.Primitive Value
  | UnimplementedPrimitive Value.Primitive
  deriving (Eq, Show)

renderEvalError :: EvalError -> String
renderEvalError (UnboundVariable name) =
  "Unbound variable: " <> name

renderEvalError (CannotEvaluate expression) =
  "Cannot evaluate: " <> SExpr.render expression

renderEvalError (NotProcedure value) =
  "Expected a procedure, received: " <> Value.render value

renderEvalError (WrongArity primitive expected received) =
  Value.render (Value.PrimitiveProcedure primitive) <>
  " expected " <> show expected <> " arguments, received " <> show received

renderEvalError (ExpectedPair primitive value) =
  Value.render (Value.PrimitiveProcedure primitive) <>
  " expected a pair, received: " <> Value.render value

renderEvalError (UnimplementedPrimitive primitive) =
  "Not implemented: " <> Value.render (Value.PrimitiveProcedure primitive)


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

eval _env expression@(SExpr.List (SExpr.Symbol "quote" : _)) =
  Left (CannotEvaluate expression)

eval env (SExpr.List (operator : operands)) = do
  procedure <- eval env operator
  arguments <- mapM (eval env) operands
  apply procedure arguments

eval _env expression =
  Left (CannotEvaluate expression)


apply :: Value -> [Value] -> Either EvalError Value
apply (Value.PrimitiveProcedure primitive) arguments
  | length arguments /= primitiveArity primitive =
      Left (WrongArity primitive (primitiveArity primitive) (length arguments))
  | otherwise = applyPrimitive primitive arguments
apply value _ =
  Left (NotProcedure value)

applyPrimitive :: Value.Primitive -> [Value] -> Either EvalError Value
applyPrimitive Value.Cons [first, rest] =
  Right (Value.Pair first rest)
applyPrimitive Value.Car [Value.Pair first _] =
  Right first
applyPrimitive Value.Car [value] =
  Left (ExpectedPair Value.Car value)
applyPrimitive Value.Cdr [Value.Pair _ rest] =
  Right rest
applyPrimitive Value.Cdr [value] =
  Left (ExpectedPair Value.Cdr value)
applyPrimitive Value.PairP [Value.Pair _ _] =
  Right (Value.Boolean True)
applyPrimitive Value.PairP [_] =
  Right (Value.Boolean False)
applyPrimitive Value.NullP [Value.EmptyList] =
  Right (Value.Boolean True)
applyPrimitive Value.NullP [_] =
  Right (Value.Boolean False)
applyPrimitive primitive _ =
  Left (UnimplementedPrimitive primitive)

primitiveArity :: Value.Primitive -> Int
primitiveArity Value.Cons = 2
primitiveArity Value.Car = 1
primitiveArity Value.Cdr = 1
primitiveArity Value.PairP = 1
primitiveArity Value.NullP = 1
primitiveArity Value.EqP = 2
primitiveArity Value.EqvP = 2


quoteDatum :: SExpr -> Value
quoteDatum (SExpr.Number number) =
  Value.Number number

quoteDatum (SExpr.Boolean boolean) =
  Value.Boolean boolean

quoteDatum (SExpr.Symbol name) =
  Value.Symbol name

quoteDatum (SExpr.List elements) =
  listToPairs (map quoteDatum elements)


listToPairs :: [Value] -> Value
listToPairs =
  foldr Value.Pair Value.EmptyList
