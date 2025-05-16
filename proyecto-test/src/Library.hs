module Library where
import PdePreludat
import Data.Foldable (maximumBy)

--pruebas con map
fDeMap1 :: [Bool]
fDeMap1 = map even [1, 2, 3, 4]

--pruebas con any
fDeAny1 :: Bool
fDeAny1 = any even [1, 2, 3, 4]

alMenosUnPar :: [Number] -> Bool
alMenosUnPar xs = any even xs

--pruebas con all
fDeAll1 :: Bool
fDeAll1 = all even [1, 2, 3, 4]

todosEnLista :: [Number] -> [Number] -> Bool
todosEnLista xs ys = all (`elem` ys) xs

--pruebas con filter





