{-# LANGUAGE ScopedTypeVariables #-}
module Sort where

import Prelude hiding ((++))
import Tip

data Nat = S Nat | Z deriving (Ord,Show, Eq)

instance Num Nat where
      Z     + b     = b
      (S a) + b     = S (a + b)
      a - Z         = a
      Z - (S b)     = error "negative result in (-)"
      (S a) - (S b) = a - b
      Z * b         = Z
      a * Z         = Z
      (S a) * b     = b + (a * b) 
      abs n         = n
      signum Z      = Z
      signum _      = S Z
      fromInteger 0 = Z
      fromInteger x = S (fromInteger x-1)  



{-(+) :: Nat -> Nat -> Nat 
Z + b = b
(S a) + b = S (a + b)
-}
(++) :: [Nat] -> [Nat] -> [Nat]
[]      ++ bs = bs
(a:as)  ++ bs = a : as ++ bs

qsort ::  [Nat] -> [Nat]
qsort []     = []
qsort (x:xs) = qsort (filterLEq x xs) ++ [x] ++ qsort (filterGT x xs)

filterLEq, filterGT :: Nat -> [Nat] -> [Nat]
filterLEq a [] = []
filterLEq a (b:bs) 
    | b<=a = b:filterLEq a bs
    | otherwise = filterLEq a bs

filterGT a [] = []
filterGT a (b:bs) 
    | b>a = b:filterGT a bs
    | otherwise = filterGT a bs

smallerEq :: [Nat] -> Nat -> Bool
smallerEq [] _     = True
smallerEq (x:xs) y = x <= y && smallerEq xs y 

bigger :: [Nat] -> Nat -> Bool
bigger [] _     = True
bigger (x:xs) y = x > y && bigger xs y 

ordered :: [Nat] -> Bool
ordered []       = True
ordered [x]      = True
ordered (x:y:xs) = x <= y && ordered (y:xs)

count :: Nat -> [Nat] -> Nat
count x [] = Z
count x (y:ys)
  | x == y = S (count x ys)
  | otherwise = count x ys


prop_count x xs = count x xs === count x (qsort xs)
prop_QSortSorts xs = bool $ ordered (qsort xs)

prop_FALSE xs = ordered (qsort xs) === ordered xs

