
import Tip
import Prelude hiding ((++),map)

(++) :: [Prop] -> [Prop] -> [Prop]
[]      ++ bs = b
(a:as)  ++ bs = a : (as ++ bs)

map :: (Prop -> Bool) -> [Prop] -> [Bool]
map _ []     = []
map f (a:as) = f a : map f as 

map1 :: (Prop -> Prop) -> [Prop] -> [Prop]
map1 _ []     = []
map1 f (a:as) = f a : map1 f as 

-- trees

data Prop
  = Not Prop
  | And [Prop]
  | Var Nat
 deriving ( Eq, Ord, Show )

type Nat = Int -- or whatever

-- eval

eval :: [Nat] -> Prop -> Bool
eval mod (Not p)  = not (eval mod p)
eval mod (And ps) = and (map (eval mod) ps)
eval mod (Var x)  = x `elem` mod

-- simp

simp :: Prop -> Prop
simp (Not p)  = simpNot (simp p)
simp (And ps) = simpAnd (map1 simp ps)
simp (Var x)  = Var x

simpNot :: Prop -> Prop
simpNot (Not p) = p
simpNot p       = Not p

simpAnd :: [Prop] -> Prop
simpAnd ps = makeAnd (flatAnd ps)

makeAnd :: [Prop] -> Prop
makeAnd [p] = p
makeAnd ps  = And ps

flatAnd :: [Prop] -> [Prop]
flatAnd (And ps : qs) = ps ++ flatAnd qs
flatAnd (p      : qs) = p : flatAnd qs
flatAnd []            = []

-- QuickCheck
{-
instance Arbitrary Prop where
  arbitrary = sized arb
   where
    arb n = frequency
            [ (1, Var `fmap` arbitrary)
            , (n, Not `fmap` arb (n-1))
            , (n, And `fmap` arbList n)
            ]
    
    arbList n =
      do k <- choose (0,0`max`(n`min`5))
         let n' | k >= 2    = n `div` k
                | otherwise = n-1
         sequence [ arb n' | i <- [1..k] ]
-}
prop_Simp ts p =
  eval ts p === eval ts (simp p)

