
import qualified Tip as Tip
import Prelude hiding ((++), map, elem, and, Bool(..), not)

-- trees

data Prop
  = Not Prop
  | And PropList
  | Var Nat
 deriving ( Eq, Ord )

type Nat = Int -- or whatever

data Bool = True | False
    deriving Eq

data PropList 
    = PNil
    | PCons Prop PropList
    deriving (Eq, Ord)

data NatList
    = NNil
    | NCons Nat NatList
    deriving Eq

data BoolList
    = BNil
    | BCons Bool BoolList
    deriving Eq

(++) :: PropList -> PropList -> PropList
PNil      ++ bs = bs
(PCons a as)  ++ bs = PCons a (as ++ bs)

map :: NatList -> PropList -> BoolList
map _ PNil     = BNil
map mod (PCons a as) = BCons (eval mod a) $ map mod as 

map1 :: PropList -> PropList
map1 PNil     = PNil
map1 (PCons a as) = PCons (simp a) $ map1 as 

elem :: Nat -> NatList -> Bool
elem x (NCons y ys) = if x == y then True else x `elem` ys 


and :: BoolList -> Bool
and BNil            = True
and (BCons True bs)    = (and bs) 
and (BCons False bs)   = False

not :: Bool -> Bool
not True = False
not False = True

-- eval

eval :: NatList -> Prop -> Bool
eval mod (Not p)  = not (eval mod p)
eval mod (And ps) = and (map mod ps)
eval mod (Var x)  = x `elem` mod

-- simp

simp :: Prop -> Prop
simp (Not p)  = simpNot (simp p)
simp (And ps) = simpAnd (map1 ps)
simp (Var x)  = Var x

simpNot :: Prop -> Prop
simpNot (Not p) = p
simpNot p       = Not p

simpAnd :: PropList -> Prop
simpAnd ps = makeAnd (flatAnd ps)

makeAnd :: PropList -> Prop
makeAnd (PCons p PNil) = p
makeAnd ps  = And ps

flatAnd :: PropList -> PropList
flatAnd (PCons (And ps) qs) = ps ++ flatAnd qs
flatAnd (PCons p qs)         = PCons p $ flatAnd qs
flatAnd PNil                 = PNil

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
--prop_simp :: NatList -> Prop -> Tip.Prop
prop_simp ts p =
  eval ts p Tip.=== eval ts (simp p)

