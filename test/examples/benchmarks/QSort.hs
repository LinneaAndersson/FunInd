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

prop_ordSub x xs = ordered (x:xs) ==> ordered (xs)
--prop_biggSub x1 x2 xs = ordered (x1:x2:xs) ==> x1==x2 || 
 
--prop_small2 y x xs = ordered (y:x:xs) ==> y <= x

prop_ppOrd x xs     = (ordered xs && smallerEq xs x) === ordered (xs++[x]) 
prop_ppOrd2 x xs    = (ordered xs && bigger xs x) ==> ordered (x:xs) 

--prop_sqf x xs = bool $ smallerEq (qsort (filterLEq x xs)) x
--prop_bqf x xs = bool $ bigger (qsort (filterGT x xs)) x

prop_countSmall xs x = count x (filterLEq x xs) === count x xs
prop_countBig xs x = count x (filterGT x xs) === 0

prop_countcount x as bs = count x as + count x bs === count x (as ++ bs) 
prop_p xs x = count x (filterLEq x xs ++ filterGT x xs) === count x xs

prop_p2 xs x = count x (filterLEq x xs ++ [x] ++ filterGT x xs) === count x (x:xs)

prop_qcons xs x = smallerEq xs x ==> (qsort (x:xs) === x:qsort xs)

prop_smallSort x xs = smallerEq xs x === smallerEq (qsort xs) x
prop_biggSort x xs  = bigger xs x === bigger (qsort xs) x 

-- QuickSort
prop_QSortSorts xs = ordered (qsort xs) === True
prop_filterOrd x xs = ordered xs === ordered ((filterLEq x xs)++(filterGT x xs))
--prop_filterCount x xs = count x xs === count x ((filterLEq x xs)++(filterGT x xs))
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

