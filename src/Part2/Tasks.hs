module Part2.Tasks where

import Util(notImplementedYet)

data BinaryOp = Plus | Minus | Times deriving (Show, Eq)

data Term = IntConstant { intValue :: Int }          -- числовая константа
          | Variable    { varName :: String }        -- переменная
          | BinaryTerm  { op :: BinaryOp, lhv :: Term, rhv :: Term } -- бинарная операция
             deriving(Show,Eq)

-- Для бинарных операций необходима не только реализация, но и адекватные
-- ассоциативность и приоритет
(|+|) :: Term -> Term -> Term
(|+|) (IntConstant i1) (IntConstant i2) = IntConstant (i1 + i2)
(|+|) t1 t2 = BinaryTerm Plus t1 t2
infixl 6 |+|

(|-|) :: Term -> Term -> Term
(|-|) (IntConstant i1) (IntConstant i2) = IntConstant (i1 - i2)
(|-|) t1 t2 = BinaryTerm Minus t1 t2
infixl 6 |-|

(|*|) :: Term -> Term -> Term
(|*|) (IntConstant i1) (IntConstant i2) = IntConstant (i1 * i2)
(|*|) t1 t2 = BinaryTerm Times t1 t2
infixl 7 |*|

-- Заменить переменную `varName` на `replacement`
-- во всём выражении `expression`
replaceVar :: String -> Term -> Term -> Term
replaceVar v r (IntConstant i) = IntConstant i
replaceVar v r (Variable var) = if v == var then r else (Variable var)
replaceVar v r (BinaryTerm op lhv rhv) = BinaryTerm op (replaceVar v r lhv) (replaceVar v r rhv)

-- Посчитать значение выражения `Term`
-- если оно состоит только из констант
evaluate :: Term -> Term
evaluate (BinaryTerm Plus (IntConstant i1) (IntConstant i2)) = IntConstant (i1 + i2)
evaluate (BinaryTerm Minus (IntConstant i1) (IntConstant i2)) = IntConstant (i1 - i2)
evaluate (BinaryTerm Times (IntConstant i1) (IntConstant i2)) = IntConstant (i1 * i2)
evaluate t = t
