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
  describe "Tests de tomates" $ do
    it "Al cortar un tomate obtengo la cantidad de partes, la décima parte de los gramos" $ do
      cortar tomate1 `shouldBe` 5
      cortar tomate3 `shouldBe` 12
    it "La cortadora suma toda las partes y le resta la cantidad" $ do
      cortadora [tomate1,tomate2,tomate3] `shouldBe` 5 + 6 + 12 - 3
    it "La rayadora de tomates raya en pedacitos por gramo" $ do
      rayar tomate1 `shouldBe` 50
    it "Debería tener máximo 100" $ do
      rayar tomate3 `shouldBe` 100
    it "Laburo manual cortando debería cortar si es maduro" $ do
      trabajaCon cortar tomate2 `shouldBe` 6
    it "Laburo manual cortando debería dejarlo entero si no es maduro" $ do
      trabajaCon rayar tomate1 `shouldBe` 1
    it "Un cocinero cocina mucho si corta en muchas partes (más de 50)" $ do
      cocinaMucho donato algunosTomates `shouldBe` True
    it "Un cocinero no cocina mucho si no corta en muchas partes (menos de 50)" $ do
      cocinaMucho german algunosTomates `shouldBe` False
  describe "Tests de ollas" $ do
    it "Una olla cuece y humedece el tomate" $ do
      olla "aceite" tomate1 `shouldBe` UnTomate ["cocido","grande","lindo","rojo"] 55