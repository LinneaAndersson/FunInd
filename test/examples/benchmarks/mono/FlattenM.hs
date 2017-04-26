module Tree where

import Tip
import Utils.Types (Tree(..),NatList(..),TreeList(..),Nat(..))

-----------------------------------

(++*) :: NatList -> NatList -> NatList
NNil      ++* bs = bs
(NCons a as)  ++* bs = NCons a $ as ++* bs

concatMapF0 :: TreeList -> NatList
concatMapF0 TNil     = NNil
concatMapF0 (TCons a as) = (flatten0 a) ++* (concatMapF0 as)

-----------------------------------

flatten0 :: Tree -> NatList
flatten0 Niil          = NNil
flatten0 (Node p x q) = flatten0 p ++* (NCons x NNil) ++* flatten0 q

flatten1 :: TreeList -> NatList
flatten1 TNil                  = NNil
flatten1 (TCons Niil ps) = flatten1 ps
flatten1 (TCons (Node Niil x q) ps) = NCons x $ flatten1 (TCons q ps)
flatten1 (TCons (Node p x q) ps) = flatten1 (TCons p $ TCons (Node Niil x q) ps)

flatten2 :: Tree -> NatList -> NatList
flatten2 Niil          ys = ys
flatten2 (Node p x q) ys = flatten2 p (NCons x $ flatten2 q ys)

flatten3 :: Tree -> NatList
flatten3 Niil                     = NNil
flatten3 (Node (Node p x q) y r) = flatten3 (Node p x (Node q y r))
flatten3 (Node Niil x q)          = NCons x $ flatten3 q

--------------------------------------------------------------------------------


prop_Flatten1 p =  
  flatten1 (TCons p TNil) === flatten0 p


prop_Flatten1List ps =
  flatten1 ps === concatMapF0 ps


prop_Flatten2 p = 
  flatten2 p NNil === flatten0 p


prop_PROVE_FALSE p a =
  flatten3 (Node p a p) === flatten0 p

prop_Flatten3 p = 
  flatten3 p === flatten0 p


