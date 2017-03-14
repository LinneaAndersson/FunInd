{-# LANGUAGE ScopedTypeVariables #-}
module Sort where

import Tip
--import Data.List hiding (sort, insert)
import Utils.SortUtils 


qsort ::  [Int] -> [Int]
qsort []     = []
qsort (x:xs) = qsort (filter (<=x) xs) ++ [x] ++ qsort (filter (>x) xs)

-- QuickSort
prop_QSortSorts xs = ordered (qsort xs) === True
prop_QSortCount x xs = count x (qsort xs) === count x xs
--prop_QSortPermutes xs = qsort xs `isPermutation` xs === True
--prop_QSortIsSort xs = qsort xs === sort xs
