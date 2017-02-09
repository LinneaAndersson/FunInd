module Int where

import Tip
import Data.List

prop_a_1 :: [Integer] -> [Integer] -> [Integer] -> Prop
prop_a_1 a b c =   a ++ (b ++ c)  === (a ++ b) ++ c

prop_comm :: [Integer] -> [Integer] -> Prop
prop_comm a b = a ++ b === b ++ a
