module Part4.Tasks where

import Util(notImplementedYet)

-- Перевёрнутый связный список -- хранит ссылку не на последующию, а на предыдущую ячейку
data ReverseList a = REmpty | (ReverseList a) :< a
infixl 5 :<

-- Функция-пример, делает из перевёрнутого списка обычный список
-- Использовать rlistToList в реализации классов запрещено =)
rlistToList :: ReverseList a -> [a]
rlistToList lst =
    reverse (reversed lst)
    where reversed REmpty = []
          reversed (init :< last) = last : reversed init

-- Реализуйте обратное преобразование
listToRlist :: [a] -> ReverseList a
listToRlist lst =
    listToRlist_ $ reverse lst
    where listToRlist_ [] = REmpty
          listToRlist_ (x : xs) = listToRlist_ xs :< x

-- Реализуйте все представленные ниже классы (см. тесты)
instance Show a => Show (ReverseList a) where
    showsPrec d = showsPrec d . rlistToList
    show = show . rlistToList
instance Eq a => Eq (ReverseList a) where
    (==) REmpty REmpty = True
    (==) _ REmpty = False
    (==) REmpty _ = False
    (==) (xs :< x) (ys :< y) = x == y && xs == ys
--    xs == ys = rlistToList xs == rlistToList ys
--    (xs :< x) == (ys :< y) = x == y && xs == ys
--    (/=) = notImplementedYet
instance Semigroup (ReverseList a) where
--    xs <> ys = listToRlist $ (++) (rlistToList xs) (rlistToList ys)
    REmpty <> xs = xs
    xs <> REmpty = xs
    xs <> (ys :< y) = (xs <> ys) :< y
instance Monoid (ReverseList a) where
    mempty = REmpty
--    xs `mappend` ys = listToRlist $ (++) (rlistToList xs) (rlistToList ys)
    xs `mappend` ys = xs <> ys
instance Functor ReverseList where
--    fmap f = listToRlist . map f . rlistToList
    fmap f REmpty = REmpty
    fmap f (xs :< x) = (fmap f xs) :< (f x)
instance Applicative ReverseList where
    pure a = REmpty :< a
    REmpty <*> xs = REmpty
    _ <*> REmpty = REmpty
--    xs <*> ys = listToRlist $ (<*>) (rlistToList xs) (rlistToList ys)
    (fs :< f) <*> xs = (fs <*> xs) `mappend` (fmap f xs)
instance Monad ReverseList where
    REmpty >>= _ = REmpty
    (xs :< x) >>= f = (xs >>= f) `mappend` (f x)
