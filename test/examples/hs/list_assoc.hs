module Int where

import Tip
--import Data.List

data List = Nil | Cons Integer List

pList :: List -> List -> List
pList Nil           bs = bs
pList (Cons a as)   bs = Cons a (pList as bs)

prop_a_1 :: List -> List -> List -> Prop
prop_a_1 a b c =   (pList a (pList b c))  === (pList (pList a b) c)


--eq :: List -> List -> Bool
--eq Nil Nil = True
--eq (Cons x xs) (Cons y ys) = eqBool x y && eq xs ys
--eq _           _           = False
--eqBool False 

--eqBool True y = y
--eqBool False False = True
--eqBool _ _ = False
