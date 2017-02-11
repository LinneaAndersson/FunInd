module Utils where

-- case of for monadic bool
mcase :: (Monad m) => m Bool -> m a -> m a -> m a
mcase mbool t f = do
    bool <- mbool
    if bool then t else f

mwhen :: (Monad m) => m Bool -> m () -> m ()
mwhen b = (flip $ mcase b) (return ())

munless :: (Monad m) => m Bool -> m () -> m ()
munless b = mcase b (return ())

splitLine :: Char -> String -> [String]
splitLine _ [] = []
splitLine c xs = takeWhile (c /= ) xs : splitLine c (dropWhile (c /= ) xs)
