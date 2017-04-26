{-# LANGUAGE ScopedTypeVariables #-}
module Sort where

import Tip
import Utils.Types

------------------------------------------
(<=*) :: Nat -> Nat -> Bool
Zero      <=* b         = True
a         <=* Zero      = False   
(Succ a)  <=* (Succ b)  = a <=* b

(++*) :: NatList -> NatList -> NatList
NNil      ++* bs = bs
(NCons a as)  ++* bs = NCons a $ as ++* bs

(+*) :: Nat -> Nat -> Nat
Zero      +* b = b
(Succ a)  +* b =Succ $ a +* b


---------------------------------------------

qsort ::  NatList -> NatList
qsort NNil     = NNil
qsort (NCons x xs) = qsort (filterLEq x xs) ++* (NCons x NNil) ++* qsort (filterGT x xs)

filterLEq, filterGT :: Nat -> NatList -> NatList
filterLEq a NNil = NNil
filterLEq a (NCons b bs) 
    | b <=* a = NCons b $ filterLEq a bs
    | otherwise = filterLEq a bs

filterGT a NNil = NNil
filterGT a (NCons b bs) 
    | not (b <=* a) = NCons b $ filterGT a bs
    | otherwise = filterGT a bs

smallerEq :: NatList -> Nat -> Bool
smallerEq NNil _     = True
smallerEq (NCons x xs) y = x <=* y && smallerEq xs y 

bigger :: NatList -> Nat -> Bool
bigger NNil _     = True
bigger (NCons x xs) y = (not (x <=* y)) && bigger xs y 

ordered :: NatList -> Bool
ordered NNil       = True
ordered (NCons x NNil)      = True
ordered (NCons x (NCons y xs)) = x <=* y && ordered (NCons y xs)

count :: Nat -> NatList -> Nat
count x NNil = Zero
count x (NCons y ys)
  | x == y = (Succ Zero) +* count x ys
  | otherwise = count x ys


prop_count x xs = count x xs === count x (qsort xs)
prop_QSortSorts xs = bool $ ordered (qsort xs)

prop_FALSE xs = ordered (qsort xs) === ordered xs

