module Tree where

import Tip

data Nat = Z | S Nat
data List = Nul | Cons Nat List
data TreeList = TNul | TreeCons Tree TreeList
data Tree = Node (Tree) Nat (Tree) | Nil

--------------------------------------------------------------------------------

concat' :: List -> List -> List
concat' Nul ys           = ys
concat' (Cons x xs) ys   = Cons x (concat' xs ys) 

concatMap' :: TreeList -> List
concatMap' TNul               = Nul
concatMap' (TreeCons t ts)    = concat' (flatten0 t) (concatMap' ts) 

flatten0 :: Tree -> List
flatten0 Nil          = Nul
flatten0 (Node p x q) =concat' (concat' (flatten0 p) (Cons x Nul)) (flatten0 q)

flatten1 :: TreeList -> List
flatten1 TNul                           = Nul
flatten1 (TreeCons Nil              ps) = flatten1 ps
flatten1 (TreeCons (Node Nil x q)   ps) = Cons x $ flatten1 (TreeCons q ps)
flatten1 (TreeCons (Node p x q)     ps) = flatten1 $ TreeCons p  (TreeCons (Node Nil x q) ps)

flatten2 :: Tree -> List -> List
flatten2 Nil          ys = ys
flatten2 (Node p x q) ys = flatten2 p (Cons x (flatten2 q ys))

flatten3 :: Tree -> List
flatten3 Nil                     = Nul
flatten3 (Node (Node p x q) y r) = flatten3 (Node p x (Node q y r))
flatten3 (Node Nil x q)          = Cons x $ flatten3 q

--------------------------------------------------------------------------------

prop_Flatten1 p =
  flatten1 (TreeCons p TNul) === flatten0 p

prop_Flatten1List ps =
  flatten1 ps === concatMap' ps

prop_Flatten2 :: Tree -> Prop
prop_Flatten2 p =
  flatten2 p Nul === flatten0 p

prop_Flatten3 p =
  flatten3 p === flatten0 p

--------------------------------------------------------------------------------
{-
swap :: Int -> Int -> Tree -> Tree 
swap a b Nil          = Nil
swap a b (Node p x q) = Node (swap a b p) x' (swap a b q)
 where
  x' | x == a  = b
     | x == b  = a
     | otherwise = x

prop_SwapAB p a b =
  a `elem` flatten0 p ==>
  b `elem` flatten0 p ==>
  a `elem` flatten0 (swap a b p) .&&.
  b `elem` flatten0 (swap a b p)
-}
