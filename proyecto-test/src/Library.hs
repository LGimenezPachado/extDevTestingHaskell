module Library where
import PdePreludat
import Data.Foldable (maximumBy)

--map tests [1]
--1.1
fDeMap1 :: [Bool]
fDeMap1 = map even [1, 2, 3, 4]

--1.2
sumarPalabras :: [String] -> Number
sumarPalabras = sum . map length

--any tests [2]

--2.1
fDeAny1 :: Bool
fDeAny1 = any even [1, 2, 3, 4]

--2.2
alMenosUnPar :: [Number] -> Bool
alMenosUnPar xs = any even xs

--all tests [3]
--3.1
fDeAll1 :: Bool
fDeAll1 = all even [1, 2, 3, 4]

--3.2
todosEnLista :: [Number] -> [Number] -> Bool
todosEnLista xs ys = all (`elem` ys) xs

--filter tests [4]
--4.1
fDeFilter :: [Number]
fDeFilter = filter even [2, 3, 4, 5, 6]

--higher-order function tests [5]
--5.1 (w/map)
listaDeFunciones :: [Number -> Number]
listaDeFunciones = map (+) [1, 2, 3]

--5.2 (wo/map)
sumarDesde :: Number -> [Number -> Number]
sumarDesde n = (n +) : sumarDesde (n + 1)

--fold tests [6]
--Intercalando operaciones binarias [6.1]
--6.1.1
ejemploSuma :: Number
ejemploSuma = foldl1 (+) [4, 8, 1]
-- Resultado esperado: 13

--6.1.2
ejemploProducto :: Number
ejemploProducto = foldl1 (*) [4, 5, 8]
-- Resultado esperado: 160

--6.1.3
ejemploConcat :: [Number]
ejemploConcat = foldl1 (++) [[1, 2], [], [3, 3, 5]]
-- Resultado esperado: [1,2,3,3,5]

--6.1.4
ejemploMax :: Number
ejemploMax = foldl1 max [4, 5, 8, 3]
-- Resultado esperado: 8

--Asociatividad [6.2]
--6.2.1
ejemploDivFoldl1 :: Number
ejemploDivFoldl1 = foldl1 (/) [10, 2, 3, 4]
-- Resultado esperado: (((10 / 2) / 3) / 4) = 0.416...

ejemploDivFoldr1 :: Number
ejemploDivFoldr1 = foldr1 (/) [10, 2, 3, 4]
-- Resultado esperado: 10 / (2 / (3 / 4)) = 3.75

--Foldeando la lista vacia [6.3]
--6.3.1
sumaVacia :: Number
sumaVacia = foldl (+) 0 []
-- Resultado esperado: 0

--6.3.2
productoVacio :: Number
productoVacio = foldl (*) 1 []
-- Resultado esperado: 1

--6.3.3
concatVacia :: [Number]
concatVacia = foldl (++) [] []
-- Resultado esperado: []

--6.3.4
ejemploFoldlSemilla :: Number
ejemploFoldlSemilla = foldl (*) 1 [4, 5, 8]
-- Resultado esperado: 160

--6.3.5
ejemploFoldrSemilla :: Number
ejemploFoldrSemilla = foldr (*) 1 [4, 5, 8]
-- Resultado esperado: 160














