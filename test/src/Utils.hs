module Utils where

-- case of for monadic bool
mcase :: (Monad m) => m Bool -> m a -> m a -> m a
mcase mbool t f = do
    bool <- mbool
    if bool then t else f

splitLine :: Char -> String -> [String]
splitLine _ [] = []
splitLine c xs = (takeWhile (c /= ) xs) : (splitLine c $ dropWhile (c /= ) xs)
