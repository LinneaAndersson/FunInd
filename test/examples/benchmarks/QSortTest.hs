{-# LANGUAGE ScopedTypeVariables #-}
module Sort where

import Tip

bigger :: [Int] -> Int -> Bool
bigger [] _     = True
bigger (x:xs) y = x > y && bigger xs y 



prop_count x xs = bigger (x:xs) (x+1) === False
