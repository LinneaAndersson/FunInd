module Tip.Passes.Funny where

import Tip.Mod
import Data.List
import Tip.Core hiding (freshArgs)
import Tip.Fresh
import Tip.Types
import Tip.Parser
import Tip.Pretty
import Tip.Pretty.TFF
import Control.Monad
import Data.Either
import Tip.Funny.Property
import Tip.Funny.Utils
import Tip.Funny.Application
import Data.Maybe


--Returns The "sub"-properties of the property
test :: (PrettyVar a, Name a) =>  Theory a -> Expr a -> Fresh [Property a]
test th e = es --freshFrom es th
    where
        es = do
            apps <- findApps (thy_funcs th) e
            let mFuncs = map (\id -> find ((==) id . func_name) (thy_funcs th)) (map snd apps)
            if Nothing `elem` mFuncs then
                fail "Could not find function"
                else do
                    let funcs = catMaybes mFuncs
                    fIds <- freshIds (map fst apps)
                    sequence $ zipWith (createProperty e) fIds funcs


test1 :: (PrettyVar a, Name a) => Theory a -> Property a -> [Expr a]
test1 th p = freshFrom (createApps p) th

betterTest :: (PrettyVar a, Name a) => Theory a -> Expr a -> [(Property a , [Expr a])]
betterTest th e = freshFrom (test th e >>= \p -> zip p <$> mapM createApps p) th
