module ValueSpec (spec) where

import Number (Number(..))
import Test.Hspec
import Value

spec :: Spec
spec = describe "Value.render" $ do
  it "renders an exact integer" $
    render (Value.Number (ExactInteger 42)) `shouldBe` "42"

  it "renders a negative exact integer" $
    render (Value.Number (ExactInteger (-7))) `shouldBe` "-7"

  it "renders an inexact real" $
    render (Value.Number (InexactReal 4.2)) `shouldBe` "4.2"

  it "renders #t for True" $
    render (Boolean True) `shouldBe` "#t"

  it "renders #f for False" $
    render (Boolean False) `shouldBe` "#f"
