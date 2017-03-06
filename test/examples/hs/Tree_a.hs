module Tree_f where

import Tip
--import Data.List

data Tree = Leaf Int | Node Tree Tree
data TList = TNil | TCons Tree TList
data List = Nil | Cons Int List

tapp :: TList -> TList -> TList
tapp TNil ys         = ys
tapp (TCons x xs) ys = TCons x (tapp xs ys)

app :: List -> List -> List
app Nil ys         = ys
app (Cons x xs) ys = Cons x (app xs ys)

f0 :: Tree -> List
f0 (Leaf a)   = Cons a Nil
f0 (Node p q) = f0 p `app` f0 q

f1 :: TList -> List
f1 TNil = Nil
f1 (TCons (Leaf x) ts) = Cons x $ f1 ts
f1 (TCons (Node p q) ts) = f1 (TCons p $ TCons q ts)

proppy ps qs = f1 (ps `tapp` qs) === f1 ps `app` f1 qs
prop_test y z = f1 (TCons y TNil) === (f0 y) 
