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
  describe "Tests de esBisiesto" $ do
    it "Un año múltiplo de 400 es bisiesto" $ do
      esBisiesto 2000 `shouldBe` True
    it "Un año múltiplo de 4 y no de 100 es bisiesto" $ do
      esBisiesto 2020 `shouldBe` True
    it "Un año múltiplo de 100 pero no de 400 no es bisiesto" $ do
      esBisiesto 300 `shouldBe` False
    it "Un año que no cumpla ninguna de las anteriores no debe ser bisiesto" $ do
      esBisiesto 1997 `shouldBe` False

