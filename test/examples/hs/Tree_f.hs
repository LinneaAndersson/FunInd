module Tree_f where

import Tip
import Data.List

data Tree a = Leaf a | Node (Tree a) (Tree a)


f0 (Leaf a)   = [a]
f0 (Node p q) = f0 p ++ f0 q

f1 [] = []
f1 (Leaf x : ts) = x : f1 ts
f1 (Node p q : ts) = f1 (p : q : ts)

proppy ps qs = f1 (ps ++ qs) === f1 ps ++ f1 qs
--prop_test y = f1 ([y]) === (f0 y) 
