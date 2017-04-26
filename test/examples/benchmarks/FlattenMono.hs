module Tree where

import Tip
--import Prelude hiding ((++), (+++))

data TreeList = TNil | TCons Tree TreeList
data IntList = INil | ICons Int IntList

concatMapF0 :: TreeList -> IntList
concatMapF0 TNil     = INil
concatMapF0 (TCons a as) = (flatten0 a) ++* (concatMapF0 as)

(++*) :: IntList -> IntList -> IntList
INil      ++* bs = bs
(ICons a as)  ++* bs = ICons a $ as ++* bs

data Tree = Node (Tree) Int (Tree) | Niil deriving Eq

--------------------------------------------------------------------------------

flatten0 :: Tree -> IntList
flatten0 Niil          = INil
flatten0 (Node p x q) = flatten0 p ++* (ICons x INil) ++* flatten0 q

flatten1 :: TreeList -> IntList
flatten1 TNil                  = INil
flatten1 (TCons Niil ps) = flatten1 ps
flatten1 (TCons (Node Niil x q) ps) = ICons x $ flatten1 (TCons q ps)
flatten1 (TCons (Node p x q) ps) = flatten1 (TCons p $ TCons (Node Niil x q) ps)

flatten2 :: Tree -> IntList -> IntList
flatten2 Niil          ys = ys
flatten2 (Node p x q) ys = flatten2 p (ICons x $ flatten2 q ys)

flatten3 :: Tree -> IntList
flatten3 Niil                     = INil
flatten3 (Node (Node p x q) y r) = flatten3 (Node p x (Node q y r))
flatten3 (Node Niil x q)          = ICons x $ flatten3 q

--------------------------------------------------------------------------------


prop_Flatten1 p =  
  flatten1 (TCons p TNil) === flatten0 p


prop_Flatten1List ps =
  flatten1 ps === concatMapF0 ps


prop_Flatten2 p = 
  flatten2 p INil === flatten0 p


prop_PROVE_FALSE p a =
  flatten3 (Node p a p) === flatten0 p

prop_Flatten3 p = 
  flatten3 p === flatten0 p


