module ISort where
import Prelude hiding ((++))
import Tip

data IntList = Nil | Cons Int IntList 

(++) :: IntList -> IntList -> IntList
Nil      ++ bs = bs
(Cons a as)  ++ bs = Cons a $ as ++ bs

sort :: IntList -> IntList
sort = isort

isort ::  IntList -> IntList
isort Nil     = Nil
isort (Cons x xs) = insert x (isort xs)

insert ::  Int -> IntList -> IntList
insert x Nil                 = (Cons x Nil)
insert x (Cons y xs) | x <= y    = Cons x $ Cons y xs
                | otherwise = Cons y $ insert x xs

--------------------------------------------------------------------------------

-- Insertion sort
prop_ISortSorts xs = ordered (isort xs) === True
prop_ISortCount x xs = count x (isort xs) === count x xs

prop_FALSE xs = ordered (isort xs) === ordered xs

ordered :: IntList -> Bool
ordered Nil       = True
ordered (Cons x Nil)      = True
ordered (Cons x (Cons y xs)) = x <= y && ordered (Cons y xs)

count :: Int -> IntList -> Int
count x Nil = 0
count x (Cons y ys)
  | x == y = 1 + count x ys
  | otherwise = count x ys

