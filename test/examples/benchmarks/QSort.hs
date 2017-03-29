{-# LANGUAGE ScopedTypeVariables #-}
module Sort where

import Tip


qsort ::  [Int] -> [Int]
qsort []     = []
qsort (x:xs) = qsort (filterLEq x xs) ++ [x] ++ qsort (filterGT x xs)

filterLEq, filterGT :: Int -> [Int] -> [Int]
filterLEq a [] = []
filterLEq a (b:bs) 
    | b<=a = b:filterLEq a bs
    | otherwise = filterLEq a bs

filterGT a [] = []
filterGT a (b:bs) 
    | b>a = b:filterGT a bs
    | otherwise = filterGT a bs

smallerEq :: [Int] -> Int -> Bool
smallerEq [] _     = True
smallerEq (x:xs) y = x <= y && smallerEq xs y 

bigger :: [Int] -> Int -> Bool
bigger [] _     = True
bigger (x:xs) y = x > y && bigger xs y 

ordered :: [Int] -> Bool
ordered []       = True
ordered [x]      = True
ordered (x:y:xs) = x <= y && ordered (y:xs)

count :: Int -> [Int] -> Int
count x [] = 0
count x (y:ys)
  | x == y = 1 + count x ys
  | otherwise = count x ys

prop_countcount x as bs = count x as + count x bs === count x (as ++ bs) 

p_tmpNil y = count y [] === count y (qsort [])
p_tmp y x xs = count y (x:xs) ===  count y (filterLEq x xs) + count y [x] + count y (filterGT x xs) 

prop_QSortCount x xs = count x (qsort xs) === count x xs
{-

-}
--p_tmp2 y x xs = count y (filterLEq x xs) + count y [x] + count y (filterGT x xs) === count y (qsort (filterLEq x xs)) + count y [x] + count y (qsort(filterGT x xs))  


{-

prop_small y xs = bool $ smallerEq (filterLEq y xs) y
countpp x = count x [] === count x (qsort []) 

prop_bigg y xs = bool $ bigger (filterGT y xs) y

prop_ordSub x xs = ordered (x:xs) ==> ordered (xs)
-}
--prop_biggSub x1 x2 xs = ordered (x1:x2:xs) ==> x1==x2 || 
{- 
prop_small2 y x xs = ordered (y:x:xs) ==> y <= x

prop_ppOrd x xs     = (ordered xs && smallerEq xs x) ==> ordered (xs++[x]) 
prop_ppOrd1 x xs     = ordered (xs++[x]) ==> (ordered xs && smallerEq xs x)  
prop_ppOrd2 x xs    = (ordered xs && bigger xs x) ==> ordered (x:xs) 

prop_sqf x xs = bool $ smallerEq (qsort (filterLEq x xs)) x
prop_bqf x xs = bool $ bigger (qsort (filterGT x xs)) x

prop_countSmall xs x = count x (filterLEq x xs) === count x xs
prop_countBig xs x = count x (filterGT x xs) === 0
-}
--prp_pp a b c = (a ++ b) ++ c === a ++ (b ++) 
--prop_countcount x as bs = count x as + count x bs === count x (as ++ bs) 
{-
prop_p xs x = count x (filterLEq x xs ++ filterGT x xs) === count x xs

prop_p2 xs x = count x (filterLEq x xs ++ [x] ++ filterGT x xs) === count x (x:xs)

prop_qcons xs x = smallerEq xs x ==> (qsort (x:xs) === x:qsort xs)

prop_smallSort x xs = smallerEq xs x === smallerEq (qsort xs) x
prop_biggSort x xs  = bigger xs x === bigger (qsort xs) x 

prop_sesese as bs x =  smallerEq as x && smallerEq bs x ==> smallerEq (as ++ bs) x
prop_sesese' as bs x =  smallerEq (as ++ bs) x ==> smallerEq as x && smallerEq bs x  

prop_seseseb as bs x =  bigger as x && bigger bs x ==> bigger (as ++ bs) x
prop_seseseb' as bs x =  bigger (as ++ bs) x ==> bigger as x && bigger bs x   

p_countSort x y ys = count x (qsort [x]) === count x [x]
p_count1 x y ys = x==y ==> (count x (y:ys) == (1 + count x ys))
p_countSort1 x y ys = x==y ==> (count x (qsort (y:ys)) == (1 + count x (qsort ys)))
p_countSort1' x y ys = x/=y ==> (count x (qsort (y:ys))== (count x (qsort ys)))



prop_filterOrd x xs = ordered xs === ordered ((filterLEq x xs)++(filterGT x xs))
-}
prop_QSortSorts xs = bool $ ordered (qsort xs)
{-
prop_QSortPermutes xs = qsort xs `isPermutation` xs === True
prop_QSortIsSort xs = qsort xs === sort xs
-}
