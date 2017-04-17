module Snd where

import Tip

snd1 :: [Int] -> [Int] -> [Int]
snd1 []     bs = bs
snd1 (a:as) bs = snd1 as bs

prop_snd y = snd1 y y === y
