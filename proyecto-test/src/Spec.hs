module Spec where
import PdePreludat
import Library
import Test.Hspec

correrTests :: IO ()
correrTests = hspec $ do
  
  describe "fDeMap1" $ do
    it "devuelve la paridad de los números del 1 al 4" $ do
      fDeMap1 `shouldBe` [False, True, False, True]

  describe "fDeAny1" $ do
    it "devuelve True si al menos un número del 1 al 4 es par" $ do
      fDeAny1 `shouldBe` True

  describe "fDeAll1" $ do
    it "devuelve False si no todos los números del 1 al 4 son pares" $ do
      fDeAll1 `shouldBe` False

  describe "alMenosUnPar" $ do
    it "Devuelve True si hay al menos un número par" $ do
      alMenosUnPar [1, 2, 3] `shouldBe` True

    it "Devuelve False si no hay números pares" $ do
      alMenosUnPar [1, 3, 5] `shouldBe` False

    it "Devuelve False con lista vacía" $ do
      alMenosUnPar [] `shouldBe` False

  describe "todosEnLista" $ do
    it "Devuelve True si todos los elementos están en la otra lista" $ do
      todosEnLista [1, 2] [1, 2, 3] `shouldBe` True

    it "Devuelve False si al menos uno no está en la otra lista" $ do
      todosEnLista [1, 4] [1, 2, 3] `shouldBe` False

    it "Devuelve True con lista vacía (todos sus elementos están)" $ do
      todosEnLista [] [1, 2, 3] `shouldBe` True