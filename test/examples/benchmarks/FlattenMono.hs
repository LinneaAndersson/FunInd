module Tree where

import Tip
import Prelude hiding ((++))

concatMapF0 :: [Tree] -> [Int]
concatMapF0 []     = []
concatMapF0 (a:as) = (flatten0 a) ++ (concatMapF0 as)

(++) :: [Int] -> [Int] -> [Int]
[]      ++ bs = bs
(a:as)  ++ bs = a : as ++ bs

data Tree = Node (Tree) Int (Tree) | Nil deriving Eq

--------------------------------------------------------------------------------

flatten0 :: Tree -> [Int]
flatten0 Nil          = []
flatten0 (Node p x q) = flatten0 p ++ [x] ++ flatten0 q

flatten1 :: [Tree] -> [Int]
flatten1 []                  = []
flatten1 (Nil          : ps) = flatten1 ps
flatten1 (Node Nil x q : ps) = x : flatten1 (q : ps)
flatten1 (Node p x q   : ps) = flatten1 (p : Node Nil x q : ps)

flatten2 :: Tree -> [Int] -> [Int]
flatten2 Nil          ys = ys
flatten2 (Node p x q) ys = flatten2 p (x : flatten2 q ys)

flatten3 :: Tree -> [Int]
flatten3 Nil                     = []
flatten3 (Node (Node p x q) y r) = flatten3 (Node p x (Node q y r))
flatten3 (Node Nil x q)          = x : flatten3 q

--------------------------------------------------------------------------------


prop_Flatten1 p =  
  flatten1 [p] === flatten0 p


prop_Flatten1List ps =
  flatten1 ps === concatMapF0 ps


prop_Flatten2 p = 
  flatten2 p [] === flatten0 p


prop_PROVE_FALSE p a =
  flatten3 (Node p a p) === flatten0 p

prop_Flatten3 p = 
  flatten3 p === flatten0 p


