module Utils.SortUtils where
import Tip
import Data.List hiding (insert)

isPermutation :: [Int] -> [Int] -> Bool
[]     `isPermutation` ys = null ys
(x:xs) `isPermutation` ys = x `elem` ys && xs `isPermutation` delete x ys

ordered :: [Int] -> Bool
ordered []       = True
ordered [x]      = True
ordered (x:y:xs) = x <= y && ordered (y:xs)

uniqsorted []       = True
uniqsorted [x]      = True
uniqsorted (x:y:xs) = x < y && uniqsorted (y:xs)

count :: Int -> [Int] -> Int
count x [] = 0
count x (y:ys)
  | x == y = 1 + count x ys
  | otherwise = count x ys

deleteAll :: Int -> [Int] -> [Int]
deleteAll _ [] = []
deleteAll x (y:ys)
  | x == y = deleteAll x ys
  | otherwise = y:deleteAll x ys

sort :: [Int] -> [Int]
sort = isort

isort ::  [Int] -> [Int]
isort []     = []
isort (x:xs) = insert x (isort xs)

insert ::  Int -> [Int] -> [Int]
insert x []                 = [x]
insert x (y:xs) | x <= y    = x : y : xs
                | otherwise = y : insert x xs
