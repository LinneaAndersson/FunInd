
import Tip
import Prelude hiding ((++),(+++), reverse)

data IntList  = IntNil | IntCons Int IntList deriving (Eq, Ord)
data TreeList = TreeNil | TreeCons Tree TreeList deriving (Eq, Ord)

(++) :: IntList -> IntList -> IntList
IntNil          ++ bs = bs
(IntCons a as)  ++ bs = IntCons a (as ++ bs)

(+++) :: TreeList -> TreeList -> TreeList
TreeNil          +++ bs = bs
(TreeCons a as)  +++ bs = TreeCons a (as +++ bs)

reverse :: TreeList -> TreeList
reverse = qrev TreeNil
    where 
        qrev :: TreeList -> TreeList -> TreeList
        qrev ls TreeNil          = ls
        qrev ls (TreeCons x xs)  = qrev (TreeCons x ls) xs
-- trees

data Tree = Node Int TreeList
 deriving ( Eq, Ord )

-- bft1

bft1 :: Tree -> IntList
bft1 t = bftList (TreeCons t TreeNil)

bftList :: TreeList -> IntList
bftList TreeNil = IntNil
bftList ts = tops ts ++ bftList (children ts)

tops :: TreeList -> IntList
tops TreeNil                = IntNil
tops (TreeCons (Node x _) ts) = IntCons x $ tops ts

children :: TreeList -> TreeList
children TreeNil                 = TreeNil
children (TreeCons (Node _ vs) ts) = vs +++ children ts

-- bft2

bft2 :: Tree -> IntList
bft2 t = bftQueue (TreeCons t TreeNil) TreeNil

bftQueue :: TreeList -> TreeList -> IntList
bftQueue TreeNil TreeNil = IntNil
bftQueue TreeNil rs = bftQueue (reverse rs) TreeNil
bftQueue (TreeCons (Node x ps) qs) rs = IntCons x $ bftQueue qs (reverse ps +++ rs)

-- QuickCheck
{-
instance Arbitrary a => Arbitrary (Tree ) where
  arbitrary = sized arb
   where
    arb n = do x  <- arbitrary
               k  <- choose (0,0 `max` (n `min` 5))
               let n' | k >= 2    = n `div` k
                      | otherwise = n-1
               xs <- sequence [ arb n' | i <- [1..k] ]
               return (Node x xs)
-}
prop_Bft t = bft1 t === bft2 t

