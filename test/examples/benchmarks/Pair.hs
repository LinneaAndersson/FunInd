{-# LANGUAGE ScopedTypeVariables #-}
module Pair where

import Tip
import Data.List

--------------------------------------------------------------------------------
pairs :: [Int] -> [(Int, Int)]
pairs (x:y:xs) = (x,y) : pairs xs
pairs _        = []

unpair :: [(Int,Int)] -> [Int]
unpair []          = []
unpair ((x,y):xys) = x : y : unpair xys

{-# NOINLINE evens #-}
evens :: [Int] -> [Int]
evens (x:xs) = x : odds xs
evens []     = []

{-# NOINLINE odds #-}
odds :: [Int] -> [Int]
odds (x:xs) = evens xs
odds []     = []

--------------------------------------------------------------------------------

prop_PairUnpair xs =
  even (length xs) ==>
    unpair (pairs xs) === xs

prop_PairEvens xs =
  even (length xs) ==>
    map fst (pairs xs) === evens xs

prop_PairOdds xs =
--  even (length xs) ==>
    map snd (pairs xs) === odds xs

