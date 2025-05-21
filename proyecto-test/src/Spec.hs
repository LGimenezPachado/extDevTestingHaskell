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
  
  describe "fDeMap2" $ do
    it "Suma 4 a una lista" $ 
      fDeMap2 `shouldBe` [12, 11, 10]

  describe "longitudesSinPuntuacion" $ do
    it "devuelve longitudes sin contar puntuación" $
      longitudesSinPuntuacion ["hola,", "qué", "tal!"] `shouldBe` [4, 3, 3]

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

  -- HoF tests [5]
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
  
  describe "esPar (lambda)" $ do  
    it "Devuelve True si el número es par" $
      esPar 4 `shouldBe` True

    it "Devuelve False si el número es impar" $
      esPar 5 `shouldBe` False

  describe "filtrarPares (lambda en filter)" $ do
    it "Filtra correctamente los números pares" $
      filtrarPares [1,2,3,4,5,6] `shouldBe` [2,4,6]

    it "Lista vacía da resultado vacío" $
      filtrarPares [] `shouldBe` []

    it "Todos impares da lista vacía" $
      filtrarPares [1,3,5] `shouldBe` []

  describe "aplicarFunciones (lambda en map)" $ do
    let funciones = [\x -> x + 1, \x -> x * 2, \x -> x ^ 2]
    it "Aplica funciones correctamente a un número" $
      aplicarFunciones funciones 3 `shouldBe` [4,6,9] 
    
  describe "sumarUnoATodos (map + lambda)" $ do
    it "Suma 1 a cada elemento de la lista" $
      sumarUnoATodos [1,2,3] `shouldBe` [2,3,4]

  describe "sumaLista (foldl + lambda)" $ do
    it "Suma los elementos de la lista" $
      sumaLista [1,2,3,4] `shouldBe` 10

    it "Lista vacía devuelve 0" $
      sumaLista [] `shouldBe` 0

  describe "productoLista (foldr + lambda)" $ do
    it "Multiplica todos los elementos" $
      productoLista [2,3,4] `shouldBe` 24

    it "Lista vacía da 1 (neutro del producto)" $
      productoLista [] `shouldBe` 1

  describe "sumaDeCuadrados (foldl + lambda)" $ do
    it "Suma los cuadrados de los elementos" $
      sumaDeCuadrados [1,2,3] `shouldBe` 14  -- 1^2 + 2^2 + 3^2

  describe "sumaElementoAElemento (zipWith + lambda)" $ do
    it "Suma dos listas elemento a elemento" $
      sumaElementoAElemento [1,2,3] [4,5,6] `shouldBe` [5,7,9]

    it "Trunca a la lista más corta" $
      sumaElementoAElemento [1,2] [10,20,30] `shouldBe` [11,22]

  describe "mayoresQue (zipWith + lambda)" $ do
    it "Compara dos listas elemento a elemento (x > y)" $
      mayoresQue [5,2,8] [3,2,10] `shouldBe` [True, False, False]
  
  describe "myZip" $ do
    it "zips two equal-length lists" $
      myZip [1, 2, 3] ['a', 'b', 'c'] `shouldBe` [(1, 'a'), (2, 'b'), (3, 'c')]

    it "zips when first list is longer" $
      myZip [1, 2, 3, 4] ['x', 'y'] `shouldBe` [(1, 'x'), (2, 'y')]

    it "zips when second list is longer" $
      myZip [1] ['a', 'b', 'c'] `shouldBe` [(1, 'a')]

    it "zips two empty lists" $
      myZip ([] :: [Number]) ([] :: [Char]) `shouldBe` []

  describe "myZipWith" $ do
    it "zips two lists with addition" $
      myZipWith (+) [1, 2, 3] [4, 5, 6] `shouldBe` [5, 7, 9]

    it "zips two lists with multiplication" $
      myZipWith (*) [2, 3] [4, 5] `shouldBe` [8, 15]

    it "zips two lists into strings" $
      myZipWith (\a b -> a : show b) ['a', 'b'] [1, 2] `shouldBe` ["a1", "b2"]

    it "zips when one list is empty" $
      myZipWith (++) ["a", "b"] [] `shouldBe` ([] :: [String])