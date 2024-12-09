{-# LANGUAGE FlexibleInstances #-}
module Part6.Tasks where

import Util (notImplementedYet)
import Data.Map

-- Разреженное представление матрицы. Все элементы, которых нет в sparseMatrixElements, считаются нулями
data SparseMatrix a = SparseMatrix {
                                sparseMatrixWidth :: Int,
                                sparseMatrixHeight :: Int,
                                sparseMatrixElements :: Map (Int, Int) a
                         } deriving (Show, Eq)

-- Определите класс типов "Матрица" с необходимыми (как вам кажется) операциями,
-- которые нужны, чтобы реализовать функции, представленные ниже
class Matrix mx where
    matFromFunc :: Int -> Int -> (Int -> Int -> Int) -> mx
    matFromList :: [[Int]] -> mx
    matrows :: mx -> Int
    matcolumns :: mx -> Int
    matelem :: mx -> Int -> Int -> Int

    matFromFunc w h f = matFromList $ [ [ f i j | j <- [0..(w - 1)] ] | i <- [0..(h - 1)] ]

    matFromList lst = let
            w = length $ head lst
            h = length lst
            indexedMx = indexedList $ Prelude.map indexedList lst
            m = fromList [ ((i, j), x) | (i, indexedRow) <- indexedMx, (j, x) <- indexedRow ]
        in
            matFromFunc w h (\i j -> findWithDefault 0 (i, j) m)
        where
            indexedList :: [a] -> [(Int, a)]
            indexedList = zip [0..]

-- Определите экземпляры данного класса для:
--  * числа (считается матрицей 1x1)
--  * списка списков чисел
--  * типа SparseMatrix, представленного выше
instance Matrix Int where
    matFromList [[a]] = a
    matrows _ = 1
    matcolumns _ = 1
    matelem a 0 0 = a

instance Matrix [[Int]] where
    matFromList = id
    matrows = length
    matcolumns = length . head
    matelem m i j = (m !! i) !! j

instance Matrix (SparseMatrix Int) where
    matFromFunc w h f = SparseMatrix {
        sparseMatrixWidth = w,
        sparseMatrixHeight = h,
        sparseMatrixElements = fromList [ ((i, j), el) | i <- [0..(h - 1)],
                                                              j <- [0..(w - 1)],
                                                              let el = f i j,
                                                              el /= 0 ]
    }
    matrows = sparseMatrixHeight
    matcolumns = sparseMatrixWidth
    matelem m i j = findWithDefault 0 (i, j) (sparseMatrixElements m)

-- Реализуйте следующие функции
-- Единичная матрица
eye :: Matrix m => Int -> m
-- eye w = matFromList $ [ [ if i == j then 1 else 0 | j <- [0..(w - 1)] ] | i <- [0..(w - 1)] ]
eye w = matFromFunc w w $ \i j -> if i == j then 1 else 0
-- Матрица, заполненная нулями
zero :: Matrix m => Int -> Int -> m
zero w h = matFromFunc w h $ \_ _ -> 0
-- zero w h = matFromList $ [ [ 0 | j <- [0..(w - 1)] ] | i <- [0..(h - 1)] ]
-- Перемножение матриц
multiplyMatrix :: Matrix m => m -> m -> m
multiplyMatrix m1 m2 = let
        w = matcolumns m2
        h = matrows m1
    in
        matFromFunc w h (\i j -> multiplyMatrix_helper i j 0 0)
    where
        d = matcolumns m1

        multiplyMatrix_helper :: Int -> Int -> Int -> Int -> Int
        multiplyMatrix_helper i j k acc
            | k < d = multiplyMatrix_helper i j (k + 1) $ acc + (matelem m1 i k) * (matelem m2 k j)
            | otherwise = acc
-- Определитель матрицы
determinant :: Matrix m => m -> Int
determinant m = determinant_helper w m
    where
        w = matcolumns m
        h = matrows m

        determinant_helper :: Matrix m1 => Int -> m1 -> Int
        determinant_helper col m
            | col == 1 = matelem m 0 0
            | otherwise = Prelude.foldl (\s (a, d) -> s + a * d) 0
                            $ Prelude.map (\i -> ((-1)^i * matelem m 0 i, determinant $ minor m 0 i)) [0..(w - 1)]

        minor :: Matrix m1 => m1 -> Int -> Int -> m1
        minor m r c = matFromFunc (w - 1) (h - 1) $ minor_helper m r c

        minor_helper :: Matrix m1 => m1 -> Int -> Int -> Int -> Int -> Int
        minor_helper m r c i j
            | i < r && j < c = matelem m i j
            | i < r && j >= c = matelem m i (j + 1)
            | i >= r && j < c = matelem m (i + 1) j
            | otherwise = matelem m (i + 1) (j + 1)
