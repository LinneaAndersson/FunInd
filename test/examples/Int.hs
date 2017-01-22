module Int where

import Tip


prop_div_mod :: Int -> Int -> Prop
prop_div_mod x y = x + y  === y + x
