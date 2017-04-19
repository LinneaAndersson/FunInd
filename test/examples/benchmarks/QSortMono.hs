{-# LANGUAGE ScopedTypeVariables #-}
module Sort where

import Prelude hiding ((++))
import Tip

data IntList = Nil | Cons Int IntList

(++) :: IntList -> IntList -> IntList
Nil      ++ bs = bs
(Cons a as)  ++ bs = Cons a $ as ++ bs

qsort ::  IntList -> IntList
qsort Nil     = Nil
qsort (Cons x xs) = qsort (filterLEq x xs) ++ (Cons x Nil) ++ qsort (filterGT x xs)

filterLEq, filterGT :: Int -> IntList -> IntList
filterLEq a Nil = Nil
filterLEq a (Cons b bs) 
    | b<=a = Cons b $ filterLEq a bs
    | otherwise = filterLEq a bs

filterGT a Nil = Nil
filterGT a (Cons b bs) 
    | b>a = Cons b $ filterGT a bs
    | otherwise = filterGT a bs

smallerEq :: IntList -> Int -> Bool
smallerEq Nil _     = True
smallerEq (Cons x xs) y = x <= y && smallerEq xs y 

bigger :: IntList -> Int -> Bool
bigger Nil _     = True
bigger (Cons x xs) y = x > y && bigger xs y 

ordered :: IntList -> Bool
ordered Nil       = True
ordered (Cons x Nil)      = True
ordered (Cons x (Cons y xs)) = x <= y && ordered (Cons y xs)

count :: Int -> IntList -> Int
count x Nil = 0
count x (Cons y ys)
  | x == y = 1 + count x ys
  | otherwise = count x ys


prop_count x xs = count x xs === count x (qsort xs)
prop_QSortSorts xs = bool $ ordered (qsort xs)

prop_FALSE xs = ordered (qsort xs) === ordered xs

