module ISort where
import Prelude hiding ((++))
import Tip

(++) :: [Int] -> [Int] -> [Int]
[]      ++ bs = bs
(a:as)  ++ bs = a : as ++ bs

sort :: [Int] -> [Int]
sort = isort

isort ::  [Int] -> [Int]
isort []     = []
isort (x:xs) = insert x (isort xs)

insert ::  Int -> [Int] -> [Int]
insert x []                 = [x]
insert x (y:xs) | x <= y    = x : y : xs
                | otherwise = y : insert x xs

--------------------------------------------------------------------------------

-- Insertion sort

prop_ISortSorts xs = ordered (isort xs) === True
prop_ISortCount x xs = count x (isort xs) === count x xs

prop_FALSE xs = ordered (isort xs) === ordered xs

ordered :: [Int] -> Bool
ordered []       = True
ordered [x]      = True
ordered (x:y:xs) = x <= y && ordered (y:xs)

count :: Int -> [Int] -> Int
count x [] = 0
count x (y:ys)
  | x == y = 1 + count x ys
  | otherwise = count x ys

