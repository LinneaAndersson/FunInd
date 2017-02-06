module Int where

import Tip

data Nat = Z | S Nat
data A = Nat

p :: Nat -> Nat -> Nat
p Z b = b
p (S a) b = S (p a b)

prop_a_1 :: Nat -> Nat -> Prop
prop_a_1 b c = forAll (\a ->  p a (p b c) === p (p a b) c ==> forAll (\a' -> a'=== S a ==>  p a' (p b c) === p (p a' b) c ))
