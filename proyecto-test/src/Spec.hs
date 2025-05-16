{-# LANGUAGE BlockArguments #-}
module Spec where
import PdePreludat
import Library
import Test.Hspec

correrTests :: IO ()
correrTests = hspec $ do
--map tests [1]
--1.1
  describe "fDeMap1" $ do
    it "Devuelve la paridad de los números del 1 al 4" $ 
      fDeMap1 `shouldBe` [False, True, False, True]
--1.2
  describe "sumarPalabras" $ do
    it "Suma las longitudes de todas las palabras" $ 
      sumarPalabras ["paradigmas", "rules", "the", "world"] `shouldBe` 23

--any tests [2]
--2.1
  describe "fDeAny1" $ do
    it "Devuelve True si al menos un número del 1 al 4 es par" $ 
      fDeAny1 `shouldBe` True
--2.2
  describe "alMenosUnPar" $ do
    it "Devuelve True si hay al menos un número par" $ 
      alMenosUnPar [1, 2, 3] `shouldBe` True

    it "Devuelve False si no hay números pares" $ 
      alMenosUnPar [1, 3, 5] `shouldBe` False

    it "Devuelve False con lista vacía" $ 
      alMenosUnPar [] `shouldBe` False

--all tests [3]
--3.1
  describe "fDeAll1" $ do
    it "Devuelve False si no todos los números del 1 al 4 son pares" $ 
      fDeAll1 `shouldBe` False
--3.2
  describe "todosEnLista" $ do
    it "Devuelve True si todos los elementos están en la otra lista" $ 
      todosEnLista [1, 2] [1, 2, 3] `shouldBe` True

    it "Devuelve False si al menos uno no está en la otra lista" $ 
      todosEnLista [1, 4] [1, 2, 3] `shouldBe` False

    it "Devuelve True con lista vacía (todos sus elementos están)" $ 
      todosEnLista [] [1, 2, 3] `shouldBe` True
--filter tests [4]
--4.1
  describe "fDeFilter" $ do
    it "Filtra los números pares de la lista" $ 
      fDeFilter `shouldBe` [2,4,6]

-- higher-order function tests [5]
--5.1 (w/map)
  describe "listaDeFunciones" $ do
    it "Aplica la primera función a 10 (debería sumar 1)" $ 
      (listaDeFunciones !! 0) 10 `shouldBe` 11

    it "Aplica la segunda función a 10 (debería sumar 2)" $ 
      (listaDeFunciones !! 1) 10 `shouldBe` 12

    it "Aplica la tercera función a 10 (debería sumar 3)" $ 
      (listaDeFunciones !! 2) 10 `shouldBe` 13

--5.2 (wo/map)
  describe "sumarDesde" $ do
    it "La primera función de sumarDesde 3 aplicada a 2 da 5" $ 
      (head (sumarDesde 3)) 2 `shouldBe` 5

    it "La segunda función suma 4 al 2" $ 
      (sumarDesde 3 !! 1) 2 `shouldBe` 6

    it "La tercera función suma 5 al 2" $ 
      (sumarDesde 3 !! 2) 2 `shouldBe` 7

--fold tests [6]
--Intercalando operaciones binarias
--6.1
 describe "Intercalando operaciones binarias" $ do
    it "ejemploSuma: foldl1 (+) [4, 8, 1] == 13" $ 
      foldl1 (+) [4, 8, 1] `shouldBe` 13

    it "ejemploProducto: foldl1 (*) [4, 5, 8] == 160" $ 
      foldl1 (*) [4, 5, 8] `shouldBe` 160

    it "ejemploConcat: foldl1 (++) [[1,2], [], [3,3,5]] == [1,2,3,3,5]" $ 
      foldl1 (++) [[1,2], [], [3,3,5]] `shouldBe` [1,2,3,3,5]

    it "ejemploMax: foldl1 max [4, 5, 8, 3] == 8" $ 
      foldl1 max [4, 5, 8, 3] `shouldBe` 8

--Asociatividad [6.2]
--6.2
  describe "Asociatividad" $ do
    it "ejemploDivFoldl1: foldl1 (/) [10,2,3,4] == (((10/2)/3)/4)" $ 
      foldl1 (/) [10, 2, 3, 4] `shouldBe` (((10 / 2) / 3) / 4)

    it "ejemploDivFoldr1: foldr1 (/) [10,2,3,4] == 10 / (2 / (3 / 4))" $ 
      foldr1 (/) [10, 2, 3, 4] `shouldBe` (10 / (2 / (3 / 4)))

--Foldeando la lista vacìa
--6.3
  describe "Foldeando la lista vacía" $ do
    it "sumaVacia: foldl (+) 0 [] == 0" $ 
      foldl (+) 0 [] `shouldBe` 0

    it "productoVacio: foldl (*) 1 [] == 1" $ 
      foldl (*) 1 [] `shouldBe` 1

    it "concatVacia: foldl (++) [] [] == []" $ 
      foldl (++) [] [] `shouldBe` ([] :: [Number])

    it "ejemploFoldlSemilla: foldl (*) 1 [4,5,8] == 160" $ 
      foldl (*) 1 [4,5,8] `shouldBe` 160

    it "ejemploFoldrSemilla: foldr (*) 1 [4,5,8] == 160" $ 
      foldr (*) 1 [4,5,8] `shouldBe` 160
  