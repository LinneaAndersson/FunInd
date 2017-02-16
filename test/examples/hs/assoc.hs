module Int where

import Tip

data Nat = Z | S Nat
data A = Nat

p :: Nat -> Nat -> Nat
p Z b = b
p (S a) b = S (p a b)

prop_a_1 :: Nat -> Nat -> Nat -> Prop
prop_a_1 a b c = p (p a b) c === p a (p b c)
