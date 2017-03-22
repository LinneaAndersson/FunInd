module MSort where

import Tip
--import Data.List hiding (sort, insert)

msortbu ::  [Int] -> [Int]
msortbu = mergingbu . map (:[])

mergingbu ::  [[Int]] -> [Int]
mergingbu []   = []
mergingbu [xs] = xs
mergingbu xss  = mergingbu (pairwise xss)

pairwise (xs:ys:xss) = xs `lmerge` ys : pairwise xss
pairwise xss         = xss

lmerge :: Ord a => [a] -> [a] -> [a]
[]     `lmerge` ys = ys
xs     `lmerge` [] = xs
(x:xs) `lmerge` (y:ys)
  | x <= y        = x : xs `lmerge` (y:ys)
  | otherwise     = y : (x:xs) `lmerge` ys

-- Bottom-up merge sort
prop_MSortBUSorts xs = ordered (msortbu xs) === True
prop_MSortBUCount x xs = count x (msortbu xs) === count x xs
--prop_MSortBUPermutes (xs :: [OrdA]) = msortbu xs `isPermutation` xs === True
--prop_MSortBUIsSort (xs :: [OrdA]) = msortbu xs === sort xs

ordered :: [Int] -> Bool
ordered []       = True
ordered [x]      = True
ordered (x:y:xs) = x <= y && ordered (y:xs)

count :: Int -> [Int] -> Int
count x [] = 0
count x (y:ys)
  | x == y = 1 + count x ys
  | otherwise = count x ys
