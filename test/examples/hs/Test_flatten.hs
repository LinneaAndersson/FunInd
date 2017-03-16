module Test_flatten where
import Tip

data Tree a = Leaf a | Node (Tree a) (Tree a)

flat0 (Leaf x) = [x]
flat0 (Node p q) = flat0 p ++ flat0 q
{-
flat1 [] = []
flat1 (Leaf x : ts) = x : flat1 ts
flat1 (Node p q : ts) = flat1 (p : q : ts)
-}

flat3 (Leaf x) = [x]
flat3 (Node (Leaf x) q) = x : flat3 q
flat3 (Node (Node p q) r) = flat3 (Node p (Node q r))

prop_flat3 p = flat0 p === flat3 p
{-
prop_flat1 p q = flat1 (p ++ q) = flat1 p ++ flat1 q
-}
