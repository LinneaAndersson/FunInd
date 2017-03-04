module Int where

import Tip
import Data.List

prop_a1 :: [Int] -> [Int] -> [Int] -> Prop
prop_a1 a b c = (a ++ b) ++ c === a ++ (b ++ c)
