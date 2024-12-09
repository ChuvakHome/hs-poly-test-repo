module Part1.Tasks where

import Util(notImplementedYet)

-- синус числа (формула Тейлора)
mySin :: Double -> Double
mySin x = _mySinTaylorSeries 15
    where
        _x = _normalizeArg x
        _x2 = _x ^ 2

        _mySinTaylorSeries :: Integer -> Double
        _mySinTaylorSeries n = _mySinTaylorSeries_acc n 1 0.0 _x

        _mySinTaylorSeries_acc :: Integer -> Integer -> Double -> Double -> Double
        _mySinTaylorSeries_acc n k s t
                                | n == k = s
                                | otherwise = _mySinTaylorSeries_acc n (k + 1) (s + t) (t * (-1) * _x2 / (2.0 * fromIntegral k) / (2.0 * (fromIntegral k) + 1))

        _normalizeArg :: Double -> Double
        _normalizeArg x
                | x < 0.0 = _normalizeArg (x + 2.0 * pi)
                | x > (2.0 * pi) = _normalizeArg (x - 2.0 * pi)
                | otherwise = x

-- косинус числа (формула Тейлора)
myCos :: Double -> Double
--myCos x = mySin (pi / 2 - x)
myCos x = _myCosTaylorSeries 15
    where
        _x = _normalizeArg x
        _x2 = _x * _x

        _myCosTaylorSeries :: Integer -> Double
        _myCosTaylorSeries n = _myCosTaylorSeries_acc n 1 0.0 1.0

        _myCosTaylorSeries_acc :: Integer -> Integer -> Double -> Double -> Double
        _myCosTaylorSeries_acc n k s t
                                | n == k = s
                                | otherwise = _myCosTaylorSeries_acc n (k + 1) (s + t) (t * (-1) * _x2 / (2.0 * (fromIntegral k) - 1) / (2.0 * fromIntegral k))

        _normalizeArg :: Double -> Double
        _normalizeArg x
                | x < 0.0 = _normalizeArg (x + 2.0 * pi)
                | x > 2.0 * pi = _normalizeArg (x - 2.0 * pi)
                | otherwise = x

-- наибольший общий делитель двух чисел
myGCD :: Integer -> Integer -> Integer
myGCD x 0 = abs x
myGCD 0 x = abs x
myGCD x y = _myGCD (abs x) (abs y)
    where
        _myGCD :: Integer -> Integer -> Integer
        _myGCD x y
            | x > y = myGCD y (x `rem` y)
            | otherwise = myGCD x (y `rem` x)

-- является ли дата корректной с учётом количества дней в месяце и
-- вискокосных годов?
isDateCorrect :: Integer -> Integer -> Integer -> Bool
isDateCorrect day 02 year = day >= 1 && day <= 28 || day == 29 && _isLeapYear year
  where
    _isLeapYear :: Integer -> Bool
    _isLeapYear year = year `rem` 4 == 0 && year `rem` 100 /= 0 || year `rem` 400 == 0
isDateCorrect 31 month year = month `elem` [1, 3, 5, 7, 8, 10, 12]
isDateCorrect day month _ = day >= 1 && day <= 30 && month >= 1 && month <= 12

-- возведение числа в степень, duh
-- готовые функции и плавающую арифметику использовать нельзя
myPow :: Integer -> Integer -> Integer
myPow n 0 = 1
myPow n m = n * myPow n (m - 1)

-- является ли данное число простым?
isPrime :: Integer -> Bool
isPrime 1 = False
isPrime n = _isPrime n 2
  where
    _isPrime :: Integer -> Integer -> Bool
    _isPrime n m
            | m < n = n `rem` m /= 0 && _isPrime n (m + 1)
            | n == m = True

type Point2D = (Double, Double)

-- рассчитайте площадь многоугольника по формуле Гаусса
-- многоугольник задан списком координат
shapeArea :: [Point2D] -> Double
--shapeArea points = notImplementedYet
shapeArea points = shapeArea_helper 0.0 points
  where
    (x0,y0) = head points

    shapeArea_helper :: Double -> [Point2D] -> Double
    shapeArea_helper acc [] = abs(acc) * 0.5
    shapeArea_helper acc [(x,y)] = shapeArea_helper (acc + x * y0 - y * x0) []
    shapeArea_helper acc ((x1,y1):(x2,y2):ps) =
      shapeArea_helper (acc + x1 * y2 - y1 * x2) ((x2,y2):ps)

-- треугольник задан длиной трёх своих сторон.
-- функция должна вернуть
--  0, если он тупоугольный
--  1, если он остроугольный
--  2, если он прямоугольный
--  -1, если это не треугольник
triangleKind :: Double -> Double -> Double -> Integer
triangleKind a b c
    | a >= (b + c) || b >= (a + c) || c >= (a + b) = -1
    | (_ang a b c) < 0 || (_ang a c b) < 0 || (_ang b c a) < 0 = 0
    | (_ang a b c) > 0 && (_ang a c b) > 0 && (_ang b c a) > 0 = 1
    | otherwise = 2
    where
        _ang :: Double -> Double -> Double -> Double
        _ang a b c = (a ^ 2 + b ^ 2 - c ^ 2) / (2 * a * b)
