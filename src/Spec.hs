module Spec where
import PdePreludat
import Library
import Test.Hspec

correrTests :: IO ()
correrTests = hspec $ do
  describe "Tests de esMultiploDe" $ do
    it "esMultiplo da cierto cuando divisor es más chico" $ do
      esMultiploDe 2 46 `shouldBe` True
    it "esMultiplo da falso cuando divisor es más grande" $ do
      esMultiploDe 46 2 `shouldBe` False
    it "esMultiplo da falso cuando no es múltiplo" $ do
      esMultiploDe 3 46 `shouldBe` False

