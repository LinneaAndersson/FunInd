module ISort where
--import Prelude hiding ((++))
import Tip


data Nat = Zero | Succ Nat

----------------------------------------

(+*) :: Nat -> Nat -> Nat
Zero      +* b = b
(Succ a)  +* b =Succ $ a +* b

(<=*) :: Nat -> Nat -> Bool
Zero      <=* b         = True
a         <=* Zero      = False   
(Succ a)  <=* (Succ b)  = a <=* b


-----------------------------------------



h_l4 x = x +* Zero === x

h_l7 x y = (Succ x) +* y === x +* (Succ y)
h_l8 x y = Succ (x +* y) === x +* (Succ y)

h_comm x y = x +* y === y +* x

lemma74 y x z = ((y +* x) <=* (z +* x)) === (y <=* z)

lemma74_p y x z = ((x +* y) <=* (x +* z)) === (y <=* z)

lemma x y z = ((y +* x) <=* (z +* x)) === ((x +* y) <=* (x +* z)) 
