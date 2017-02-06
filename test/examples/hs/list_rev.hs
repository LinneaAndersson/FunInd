module List_assoc where

--import Data.List
import Tip
data List = Nil | Cons Int List
 
qrev :: List -> List -> List
qrev Nil ys = ys
qrev (Cons x xs) ys = qrev xs (Cons x ys)

prop_rev_assoc :: List -> Prop
prop_rev_assoc a = qrev (qrev a Nil) Nil === a
