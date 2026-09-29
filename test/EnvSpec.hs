module EnvSpec (spec) where

import qualified Env
import Number (Number(..))
import Test.Hspec
import qualified Value

spec :: Spec
spec = describe "Env" $ do
  describe "empty" $ do
    it "contains no bindings" $
      Env.lookup "x" Env.empty
        `shouldBe` Nothing

  describe "fromList" $ do
    it "creates bindings" $
      let environment =
            Env.fromList
              [ ("x", Value.Number (ExactInteger 42))
              ]
      in Env.lookup "x" environment
           `shouldBe` Just (Value.Number (ExactInteger 42))

    it "can contain different kinds of values" $
      let environment =
            Env.fromList
              [ ("answer", Value.Boolean True)
              , ("name", Value.Symbol "luxury")
              ]
      in do
        Env.lookup "answer" environment
          `shouldBe` Just (Value.Boolean True)

        Env.lookup "name" environment
          `shouldBe` Just (Value.Symbol "luxury")

    it "returns Nothing for an unbound name" $
      let environment =
            Env.fromList
              [ ("x", Value.Number (ExactInteger 42))
              ]
      in Env.lookup "y" environment
           `shouldBe` Nothing

  describe "initial" $ do
    it "contains cons primitive" $
      Env.lookup "cons" Env.initial
        `shouldBe` Just (Value.PrimitiveProcedure Value.Cons)

    it "contains car primitive" $
      Env.lookup "car" Env.initial
        `shouldBe` Just (Value.PrimitiveProcedure Value.Car)

    it "contains cdr primitive" $
      Env.lookup "cdr" Env.initial
        `shouldBe` Just (Value.PrimitiveProcedure Value.Cdr)

    it "contains pair? primitive" $
      Env.lookup "pair?" Env.initial
        `shouldBe` Just (Value.PrimitiveProcedure Value.PairP)

    it "contains null? primitive" $
      Env.lookup "null?" Env.initial
        `shouldBe` Just (Value.PrimitiveProcedure Value.NullP)

    it "does not contain unbound names" $
      Env.lookup "undefined" Env.initial
        `shouldBe` Nothing
