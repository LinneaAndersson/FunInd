{-# LANGUAGE ScopedTypeVariables #-}
module Sort where

import Tip


qsort ::  [Int] -> [Int]
qsort []     = []
qsort (x:xs) = qsort (filterLEq x xs) ++ [x] ++ qsort (filterGT x xs)

filterLEq, filterGT :: Int -> [Int] -> [Int]
filterLEq a [] = []
filterLEq a (b:bs) 
    | b<=a = b:filterLEq a bs
    | otherwise = filterLEq a bs

filterGT a [] = []
filterGT a (b:bs) 
    | b>a = b:filterGT a bs
    | otherwise = filterGT a bs

smallerEq :: [Int] -> Int -> Bool
smallerEq [] _     = True
smallerEq (x:xs) y = x <= y && smallerEq xs y 

bigger :: [Int] -> Int -> Bool
bigger [] _     = True
bigger (x:xs) y = x > y && bigger xs y 

prop_small y xs = bool $ smallerEq (filterLEq y xs) y

prop_bigg y xs = bool $ bigger (filterGT y xs) y


-- QuickSort
prop_QSortSorts xs = ordered (qsort xs) === True
prop_QSortCount x xs = count x (qsort xs) === count x xs
--prop_QSortPermutes xs = qsort xs `isPermutation` xs === True
--prop_QSortIsSort xs = qsort xs === sort xs

ordered :: [Int] -> Bool
ordered []       = True
ordered [x]      = True
ordered (x:y:xs) = x <= y && ordered (y:xs)

count :: Int -> [Int] -> Int
count x [] = 0
count x (y:ys)
  | x == y = 1 + count x ys
  | otherwise = count x ys

