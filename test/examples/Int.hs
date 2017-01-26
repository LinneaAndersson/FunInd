module Int where

import Tip

data Nat = Z | S Nat

p :: Nat -> Nat -> Nat
p Z b = b
p (S a) b = S (p a b)

prop_div_mod ::  Nat -> Prop
prop_div_mod x = p Z x  === x
