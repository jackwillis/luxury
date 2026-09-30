-- Runtime environment mapping Scheme names to their current values.
module Env
  ( Env
  , bind
  , empty
  , fromList
  , initial
  , lookup
  ) where

import Prelude hiding (lookup)

import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map

import Value (Value)
import qualified Value


newtype Env = Env (Map String Value)
  deriving (Eq, Show)

empty :: Env
empty =
  Env Map.empty

initial :: Env
initial =
  fromList
    [ ("cons",  Value.PrimitiveProcedure Value.Cons)
    , ("car",   Value.PrimitiveProcedure Value.Car)
    , ("cdr",   Value.PrimitiveProcedure Value.Cdr)
    , ("pair?", Value.PrimitiveProcedure Value.PairP)
    , ("null?", Value.PrimitiveProcedure Value.NullP)
    , ("eq?",   Value.PrimitiveProcedure Value.EqP)
    , ("eqv?",  Value.PrimitiveProcedure Value.EqvP)
    ]

fromList :: [(String, Value)] -> Env
fromList bindings =
  Env (Map.fromList bindings)

lookup :: String -> Env -> Maybe Value
lookup name (Env bindings) =
  Map.lookup name bindings

bind :: String -> Value -> Env -> Env
bind name value (Env bindings) =
  Env (Map.insert name value bindings)
