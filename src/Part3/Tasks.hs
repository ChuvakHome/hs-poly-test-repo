module Part3.Tasks where

import Util (notImplementedYet)

-- Функция finc принимает на вход функцию f и число n и возвращает список чисел [f(n), f(n + 1), ...]
finc :: (Int -> a) -> Int -> [a]
finc f n = (f n):(finc f (n + 1))

-- Функция ff принимает на вход функцию f и элемент x и возвращает список [x, f(x), f(f(x)), f(f(f(x))) ...]
ff :: (a -> a) -> a -> [a]
ff f x = x:(ff f (f x))

-- Дан список чисел. Вернуть самую часто встречающуюся *цифру* в этих числах (если таковых несколько -- вернуть любую)
mostFreq :: [Int] -> Int
mostFreq lst = fst . freq . pairs . groupDigits (digits lst) $ replicate 10 0
  where
    getDigits n
          | n <= 9 = [n]
          | otherwise = getDigits (n `div` 10) ++ [n `mod` 10]

    digits = concatMap getDigits

    groupDigits [] freqLst = freqLst
    groupDigits (d:ds) freqLst = groupDigits ds $ (take d freqLst) ++ [1 + freqLst !! d] ++ (drop (d + 1) freqLst)

    pairs = pairs_acc 0 []

    pairs_acc n acc [] = acc
    pairs_acc n acc (x:xs) = pairs_acc (n + 1) (acc ++ [(n, x)]) xs

    freq (x:xs) = freq_acc xs x
    freq_acc [] acc = acc
    freq_acc (x:xs) acc = freq_acc xs $ if snd x > snd acc then x else acc

-- Дан список lst. Вернуть список элементов из lst без повторений, порядок может быть произвольным.
uniq :: (Eq a) => [a] -> [a]
uniq = uniq_helper []
    where uniq_helper lst [] = lst
          uniq_helper lst (x:xs) = if (x `elem` lst) then (uniq_helper lst xs) else (uniq_helper (x:lst) xs)

-- Функция grokBy принимает на вход список Lst и функцию F и каждому возможному
-- значению результата применения F к элементам Lst ставит в соответствие список элементов Lst,
-- приводящих к этому результату. Результат следует представить в виде списка пар.
grokBy :: (Eq k) => (a -> k) -> [a] -> [(k, [a])]
grokBy f = grokBy_helper []
  where
    grokBy_helper acc [] = acc
    grokBy_helper acc (x:xs) =
      let
        fx = f x
        eq_fx = \y -> fx == f y
        left_values = filter eq_fx xs
        right_values = filter (not . eq_fx) xs
      in
        grokBy_helper ([(fx, x:left_values)] ++ acc) right_values
