module ISort where
--import Prelude hiding ((++))
import Tip
import Utils.Types

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

--------------------------------------------------------------------------------

-- Insertion sort
prop_countCount x ys zs = count x (ys ++* zs) === count x ys +* count x zs 
prop_ISortSorts xs = ordered (isort xs) === True
prop_ISortCount x xs = count x (isort xs) === count x xs

prop_FALSE xs = ordered (isort xs) === ordered xs

ordered :: NatList -> Bool
ordered NNil       = True
ordered (NCons x NNil)      = True
ordered (NCons x (NCons y xs)) = x <=* y && ordered (NCons y xs)

count :: Nat -> NatList -> Nat
count x NNil = Zero
count x (NCons y ys)
  | x == y = (Succ Zero) +* count x ys
  | otherwise = count x ys
