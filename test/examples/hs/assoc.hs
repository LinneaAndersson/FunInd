module Int where

import Tip

data Nat = Z | S Nat
data A = Nat
p :: Nat -> Nat -> Nat
p Z b = b
p (S a) b = S (p a b)

--prop_div_mod ::  Nat -> Nat -> Nat ->  Prop
--prop_div_mod a b c =  p a (p b c)  === p (p a b) c

prop_a_1 :: Nat -> Nat -> Prop
prop_a_1 a b =  forAll (\ c -> p a (p b c)  === p (p a b) c  ==> p (S a) (p b c)  === p (p  (S a) b) c)

--prop_a_2 b = forAll (\c -> p Z (p b c) === p (p Z b) c)
