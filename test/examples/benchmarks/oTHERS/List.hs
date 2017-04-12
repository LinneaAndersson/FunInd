{-# LANGUAGE ScopedTypeVariables #-}
module List where

import Tip
import Data.List
--import Utils.SortUtils (isPermuatation, deleteAll, count)


isPermutation :: [Int] -> [Int] -> Bool
[]     `isPermutation` ys = null ys
(x:xs) `isPermutation` ys = x `elem` ys && xs `isPermutation` delete x ys

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

--------------------------------------------------------------------------------

prop_append_inj_1 xs ys zs = xs ++ zs === ys ++ zs ==> xs === ys
prop_append_inj_2 xs ys zs = xs ++ ys === xs ++ zs ==> ys === zs

prop_nub_nub xs = nub (nub xs) === nub xs

prop_elem_nub_l x xs = x `elem` xs ==> x `elem` nub xs
prop_elem_nub_r x xs = x `elem` nub xs ==> x `elem` xs
prop_count_nub  x xs = x `elem` xs ==> count x (nub xs) === 1

prop_perm_trans xs ys zs = xs `isPermutation` ys ==> ys `isPermutation` zs ==> xs `isPermutation` zs
prop_perm_refl xs        = xs `isPermutation` xs === True
prop_perm_symm xs ys     = xs `isPermutation` ys ==> ys `isPermutation` xs

prop_perm_elem x xs ys   = x `elem` xs ==> xs `isPermutation` ys ==> x `elem` ys

prop_deleteAll_count x xs = deleteAll x xs === delete x xs ==> count x xs <= 1

prop_elem x xs = x `elem` xs ==> exists (\ i -> x == xs !! i)
prop_elem_map y f xs = y `elem` map f xs ==> exists (\ x -> f x === y .&&. y `elem` xs)
