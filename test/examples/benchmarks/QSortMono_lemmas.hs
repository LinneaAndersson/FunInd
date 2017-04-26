{-# LANGUAGE ScopedTypeVariables #-}
module Sort where

import Prelude hiding ((++))
import Tip

data IntList = Nil | Cons Int IntList

(++) :: IntList -> IntList -> IntList
Nil      ++ bs = bs
(Cons a as)  ++ bs = Cons a $ as ++ bs

qsort ::  IntList -> IntList
qsort Nil     = Nil
qsort (Cons x xs) = qsort (filterLEq x xs) ++ (Cons x Nil) ++ qsort (filterGT x xs)

filterLEq, filterGT :: Int -> IntList -> IntList
filterLEq a Nil = Nil
filterLEq a (Cons b bs) 
    | b<=a = Cons b $ filterLEq a bs
    | otherwise = filterLEq a bs

filterGT a Nil = Nil
filterGT a (Cons b bs) 
    | b>a = Cons b $ filterGT a bs
    | otherwise = filterGT a bs

smallerEq :: IntList -> Int -> Bool
smallerEq Nil _     = True
smallerEq (Cons x xs) y = x <= y && smallerEq xs y 

bigger :: IntList -> Int -> Bool
bigger Nil _     = True
bigger (Cons x xs) y = x > y && bigger xs y 

ordered :: IntList -> Bool
ordered Nil       = True
ordered (Cons x Nil)      = True
ordered (Cons x (Cons y xs)) = x <= y && ordered (Cons y xs)

count :: Int -> IntList -> Int
count x Nil = 0
count x (Cons y ys)
  | x == y = 1 + count x ys
  | otherwise = count x ys


prop_pp a b c = a++(b++c) === (a++b)++c
prop_countcount x as bs = count x as + count x bs === count x (as ++ bs) 

p_tmpNil y = count y Nil === count y (qsort Nil)
p_tmp y x xs = count y (Cons x xs) ===  count y (filterLEq x xs) + count y (Cons x Nil) + count y (filterGT x xs) 

prop_QSortCount x xs = count x (qsort xs) === count x xs
--prop_smallpp2 a b c x = smallerEq (a++b++c) x === ((smallerEq a x) && ((smallerEq b x) && (smallerEq c x)))


--prop1 ys x = (smallerEq ys x) === (filterGT x ys == Nil)
--prop2 ys x = (bigger ys x) === (filterLEq x ys == Nil)

prop_ssmsms y ys x = smallerEq (Cons y ys) x === smallerEq (filterLEq y ys ++ (Cons y Nil) ++ filterGT y ys) x

prop_bbmbmb y ys x = bigger (Cons y ys) x === bigger (filterLEq y ys ++ (Cons y Nil) ++ filterGT y ys) x

--prop = xs === filterLEq xs x ++ [x] filterGt xs x 

prop_smallQ xs x = smallerEq xs x === smallerEq (qsort xs) x

prop_bigQ xs x = bigger xs x === bigger (qsort xs) x


--prop_orsAnd a b = ((ordered a) && (ordered b) &) ==> ordered (a ++ b)


prop_small x xs = bool $ smallerEq (filterLEq x xs) x
prop_big x xs = bool $ bigger (filterGT x xs) x



prop_count x xs = count x xs === count x (qsort xs)
prop_QSortSorts xs = bool $ ordered (qsort xs)

prop_FALSE xs = ordered (qsort xs) === ordered xs

