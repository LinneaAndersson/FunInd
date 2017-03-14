{-# LANGUAGE ScopedTypeVariables #-}
module Sort where

import Tip


qsort ::  [Int] -> [Int]
qsort []     = []
qsort (x:xs) = qsort (filter (<=x) xs) ++ [x] ++ qsort (filter (>x) xs)

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

