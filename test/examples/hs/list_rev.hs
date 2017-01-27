module List_assoc where

import Data.List
import Tip
prop_rev_assoc :: [Integer] -> Prop
prop_rev_assoc a = reverse (reverse a) === a ==> forAll (\b -> reverse (reverse (b:a)) === (b:a) )
