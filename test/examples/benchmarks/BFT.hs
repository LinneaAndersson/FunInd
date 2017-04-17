
import Tip
import Prelude hiding ((++),(+++))


(++) :: [Int] -> [Int] -> [Int]
[]      ++ bs = bs
(a:as)  ++ bs = a : (as ++ bs)

(+++) :: [Tree] -> [Tree] -> [Tree]
[]      +++ bs = bs
(a:as)  +++ bs = a : (as +++ bs)


-- trees

data Tree = Node Int [Tree]
 deriving ( Eq, Ord )

-- bft1

bft1 :: Tree -> [Int]
bft1 t = bftList [t]

bftList :: [Tree] -> [Int]
bftList [] = []
bftList ts = tops ts ++ bftList (children ts)

tops :: [Tree] -> [Int]
tops []              = []
tops (Node x _ : ts) = x : tops ts

children :: [Tree] -> [Tree]
children []               = []
children (Node _ vs : ts) = vs +++ children ts

-- bft2

bft2 :: Tree -> [Int]
bft2 t = bftQueue [t] []

bftQueue :: [Tree] -> [Tree] -> [Int]
bftQueue []               [] = []
bftQueue []               rs = bftQueue (reverse rs) []
bftQueue (Node x ps : qs) rs = x : bftQueue qs (reverse ps +++ rs)

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

