{-# LANGUAGE ScopedTypeVariables #-}
module Select where

import Tip
import Data.List
--import Utils.SortUtils (count, isPermutation)


isPermutation :: [Int] -> [Int] -> Bool
[]     `isPermutation` ys = null ys
(x:xs) `isPermutation` ys = x `elem` ys && xs `isPermutation` delete x ys


count :: Int -> [Int] -> Int
count x [] = 0
count x (y:ys)
  | x == y = 1 + count x ys
  | otherwise = count x ys

--------------------------------------------------------------------------------

select :: [Int] -> [(Int,[Int])]
select []     = []
select (x:xs) = (x,xs) : [(y,x:ys) | (y,ys) <- select xs]

--------------------------------------------------------------------------------

prop_Select xs =
  map fst (select xs) === xs

prop_SelectPermutations xs =
  all (`isPermutation` xs) [ y:ys | (y,ys) <- select xs ] === True

prop_SelectPermutations' xs z =
  all ((n ==) . count z) [ y:ys | (y,ys) <- select xs ] === True
 where
  n = count z xs
