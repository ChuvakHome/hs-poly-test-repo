module Part5.Tasks where

import Util(notImplementedYet)

-- Реализуйте левую свёртку
myFoldl :: (b -> a -> b) -> b -> [a] -> b
myFoldl _ init [] = init
myFoldl f init (h:t) = myFoldl f (f init h) t

-- Реализуйте правую свёртку
myFoldr :: (a -> b -> b) -> b -> [a] -> b
myFoldr _ init [] = init
myFoldr f init (h:t) = f h $ myFoldr f init t
--myFoldr f init l = myFoldr_ f init (reverse l)
--    where
--        myFoldr_ :: (a -> b -> b) -> b -> [a] -> b
--        myFoldr_ _ init [] = init
--        myFoldr_ f init (h:t) = myFoldr_ f (f h init) t

-- Используя реализации свёрток выше, реализуйте все остальные функции в данном файле

myMap :: (a -> b) -> [a] -> [b]
myMap f = myFoldr (\_a _l -> f _a : _l) []

myConcatMap :: (a -> [b]) -> [a] -> [b]
myConcatMap f = myFoldr (\_a _l -> f _a ++ _l) []

myConcat :: [[a]] -> [a]
myConcat = myFoldl (++) []

myReverse :: [a] -> [a]
myReverse = myFoldl (\_l _a -> _a:_l) []

myFilter :: (a -> Bool) -> [a] -> [a]
myFilter p = myFoldl (\_l _a -> if (p _a) then (_l ++ [_a]) else _l) []

myPartition :: (a -> Bool) -> [a] -> ([a], [a])
myPartition p = myFoldr (\_a (_xs, _ys) -> if p _a then (_a:_xs, _ys) else (_xs, _a:_ys)) ([], [])
