module Utils where


import Data.List (sort, subsequences)

-- 'case of' for monadic bool
mcase :: (Monad m) => m Bool -> m a -> m a -> m a
mcase mbool t f = do
    bool <- mbool
    if bool then t else f

-- 'when' for monadic bool
mwhen :: (Monad m) => m Bool -> m () -> m ()
mwhen b = (flip $ mcase b) (return ())

-- 'unless' for monadic bool
munless :: (Monad m) => m Bool -> m () -> m ()
munless b = mcase b (return ())

-- split a string at all matches of a given char
splitLine :: Char -> String -> [String]
splitLine _ [] = []
splitLine c xs = takeWhile (c /= ) xs : splitLine c (dropWhile (c /= ) xs)

comb :: Int -> [a] -> [[a]]
comb m =  filter ((==m) . length) . subsequences

subsets 0 _ = []
subsets n nbr = subsets (n-1) nbr ++ comb n [0..nbr-1]
