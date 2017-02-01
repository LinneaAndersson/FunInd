module Less where

import Tip



isIn :: Integer -> [Integer] -> Bool
isIn p (i:is) = p == i || (isIn p is)

less :: Integer -> [Integer] -> [Integer]
less _ [] = []
less p (i:is)
    | i < p    = i:less p is
    | otherwise = less p is

prop_in :: [Integer] -> Integer -> Prop
prop_in is p = forAll (\xs ->  xs === less p is ==> forAll (\x -> isIn x xs ==> isIn x is))
