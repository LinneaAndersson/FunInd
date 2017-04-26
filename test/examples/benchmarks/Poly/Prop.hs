
import  Tip ((===))
import Prelude hiding ((++),map,elem,Bool(..),and,not)

data Bool = True | False

(++) :: [Prop1] -> [Prop1] -> [Prop1]
[]      ++ bs = bs
(a:as)  ++ bs = a : (as ++ bs)

map :: [Nat] -> [Prop1] -> [Bool]
map _ []     = []
map mod (a:as) = (eval mod a) : map mod as 

map1 :: [Prop1] -> [Prop1]
map1 []     = []
map1 (a:as) = (simp a) : map1 as 

elem :: Nat -> [Nat] -> Bool
elem x (y:ys) = if x == y then True else x `elem` ys 

not :: Bool -> Bool 
not True = False
not False = True

and :: [Bool] -> Bool
and [] = True
and (a:as) = if a==True then and as else False

-- trees

data Prop1
  = Not Prop1
  | And [Prop1]
  | Var Nat
 deriving ( Eq, Ord)

type Nat = Int-- or whatever

-- eval

eval :: [Nat] -> Prop1 -> Bool
eval mod (Not p)  = not (eval mod p)
eval mod (And ps) = and (map mod ps)
eval mod (Var x)  = x `elem` mod

-- simp

simp :: Prop1 -> Prop1
simp (Not p)  = simpNot (simp p)
simp (And ps) = simpAnd (map1 ps)
simp (Var x)  = Var x

simpNot :: Prop1 -> Prop1
simpNot (Not p) = p
simpNot p       = Not p

simpAnd :: [Prop1] -> Prop1
simpAnd ps = makeAnd (flatAnd ps)

makeAnd :: [Prop1] -> Prop1
makeAnd [p] = p
makeAnd ps  = And ps

flatAnd :: [Prop1] -> [Prop1]
flatAnd []            = []
flatAnd (And ps : qs) = ps ++ flatAnd qs
flatAnd (p      : qs) = p : flatAnd qs

prop1_Simp ts p =
  eval ts p Tip.=== eval ts (simp p)

