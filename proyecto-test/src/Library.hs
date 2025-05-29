module Library where
import PdePreludat
import Data.Foldable (maximumBy)
import Data.Char (toUpper)

--map tests [1]
{-
map :: (a -> b) -> [a] -> [b]
map _ []     = []
map f (x:xs) = f x : map f xs 
-}
--1.1
fDeMap1 :: [Bool]
fDeMap1 = map even [1, 2, 3, 4]

fDeMap2 :: [Number]
fDeMap2 = map (+4) [8, 7, 6]

mostrarEnMayusculas :: String -> String
mostrarEnMayusculas = map toUpper

longitudesSinPuntuacion :: [String] -> [Number]
longitudesSinPuntuacion = map (length . filter (`notElem` ".,;:!?"))

--1.2
sumarPalabras :: [String] -> Number
sumarPalabras = sum . map length

--any tests [2]
{-
any :: (a -> Bool) -> [a] -> Bool
any _ [] = False
any p (x:xs) = p x || any p xs
-}
--2.1
fDeAny1 :: Bool
fDeAny1 = any even [1, 2, 3, 4]

--2.2
alMenosUnPar :: [Number] -> Bool
alMenosUnPar xs = any even xs

--all tests [3]
{-
all :: (a -> Bool) -> [a] -> Bool
all _ [] = True
all p (x:xs) = p x && all p xs
-}
--3.1
fDeAll1 :: Bool
fDeAll1 = all even [1, 2, 3, 4]

--3.2
todosEnLista :: [Number] -> [Number] -> Bool
todosEnLista xs ys = all (`elem` ys) xs

--filter tests [4]
{-
filter :: (a -> Bool) -> [a] -> [a]
filter _ [] = []
filter p (x:xs)
  | p x       = x : filter p xs
  | otherwise = filter p xs
-}
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
{- 
foldl :: (b -> a -> b) -> b -> [a] -> b
foldl _ z []     = z
foldl f z (x:xs) = foldl f (f z x) xs

foldr :: (a -> b -> b) -> b -> [a] -> b
foldr _ z []     = z
foldr f z (x:xs) = f x (foldr f z xs)
-}
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

-- zip/zipWith [7]
{-
zip :: [a] -> [b] -> [(a,b)]
zip [] _ = []
zip _ [] = []
zip (x:xs) (y:ys) = (x,y) : zip xs ys

zipWith :: (a -> b -> c) -> [a] -> [b] -> [c]
zipWith _ [] _ = []
zipWith _ _ [] = []
zipWith f (x:xs) (y:ys) = f x y : zipWith f xs ys
-}

-- Combina dos listas en una lista de pares [7.1]
myZip :: [a] -> [b] -> [(a, b)]
myZip [] _ = []
myZip _ [] = []
myZip (x:xs) (y:ys) = (x, y) : myZip xs ys

-- Aplica una función a pares de elementos de dos listas [7.2]
myZipWith :: (a -> b -> c) -> [a] -> [b] -> [c]
myZipWith _ [] _ = []
myZipWith _ _ [] = []
myZipWith f (x:xs) (y:ys) = f x y : myZipWith f xs ys

--Expresiones lambda [8]
--
-- Devuelve True si el número es par (usando lambda) [8.1]
esPar :: Number -> Bool
esPar = \x -> x `mod` 2 == 0

--
-- Filtra los pares de una lista usando una lambda directamente [8.2]
filtrarPares :: [Number] -> [Number]
filtrarPares = filter (\x -> x `mod` 2 == 0)

--
-- Aplica una lista de funciones lambda a un número [8.3]
aplicarFunciones :: [Number -> Number] -> Number -> [Number]
aplicarFunciones fs x = map (\f -> f x) fs

-- Suma 1 a cada elemento usando map y lambda [8.4]
sumarUnoATodos :: [Number] -> [Number]
sumarUnoATodos = map (\x -> x + 1)

-- Suma todos los elementos de una lista usando foldl y lambda [8.5]
sumaLista :: [Number] -> Number
sumaLista = foldl (\acum x -> acum + x) 0

-- Multiplica todos los elementos usando foldr y lambda [8.6]
productoLista :: [Number] -> Number
productoLista = foldr (\x acum -> x * acum) 1

-- Eleva cada número al cuadrado y lo suma (x^2 + y^2 + ...) [8.7]
sumaDeCuadrados :: [Number] -> Number
sumaDeCuadrados = foldl (\acum x -> acum + x^2) 0

-- Suma elemento a elemento dos listas [8.8]
sumaElementoAElemento :: [Number] -> [Number] -> [Number]
sumaElementoAElemento = zipWith (\x y -> x + y)

-- Compara dos listas elemento a elemento y devuelve True si x > y [8.9]
mayoresQue :: [Number] -> [Number] -> [Bool]
mayoresQue = zipWith (\x y -> x > y)

--Funciones compuestas [9]

cuadradosPares :: [Number] -> [Number]
cuadradosPares = map (^2) . filter even

empiezaConVocal :: String -> Bool
empiezaConVocal (x:_) = x `elem` "aeiouAEIOU"
empiezaConVocal _     = False

sumarVocales :: [String] -> Number
sumarVocales = sum . map length . filter empiezaConVocal

procesarTexto :: String -> String
procesarTexto = reverse . map toUpper . filter (/= ' ')











