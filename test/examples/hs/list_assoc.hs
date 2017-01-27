module Int where

import Tip
import Data.List

prop_a_1 :: [Integer] -> [Integer] -> Prop
prop_a_1 a b =  forAll (\ c -> a ++ (b ++ c)  === (a ++ b) ++ c  ==> forAll (\d -> (d:a) ++ (b ++ c)  === ( (d:a) ++  b) ++  c))
