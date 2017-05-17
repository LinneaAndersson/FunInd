module ISort where
--import Prelude hiding ((++))
import Tip


data Nat = Zero | Succ Nat

data NatList = NNil | NCons Nat NatList

----------------------------------------

(++*) :: NatList -> NatList -> NatList
NNil      ++* bs = bs
(NCons a as)  ++* bs = NCons a $ as ++* bs

(+*) :: Nat -> Nat -> Nat
Zero      +* b = b
(Succ a)  +* b =Succ $ a +* b

(<=*) :: Nat -> Nat -> Bool
Zero      <=* b         = True
a         <=* Zero      = False   
(Succ a)  <=* (Succ b)  = a <=* b


sort :: NatList -> NatList
sort = isort

isort ::  NatList -> NatList
isort NNil     = NNil
isort (NCons x xs) = insert x (isort xs)

insert ::  Nat -> NatList -> NatList
insert x NNil                 = (NCons x NNil)
insert x (NCons y xs) | x <=* y    = NCons x $ NCons y xs
                | otherwise = NCons y $ insert x xs



-------------------------------------------

h_l30 x =  bool (x <=* x) 

h_l40 x y  = (insert x (NCons x y)) === (NCons x (NCons x y))



lemma76 x y z = (insert y (x ++* (NCons y z))) ===
    ((insert y x) ++* (NCons y z))
