module Tip.Passes.Funny where

import Tip.Mod
import Data.List
import Tip.Core hiding (freshArgs)
import Tip.Fresh
import Tip.Types
import Tip.Parser
import Tip.Pretty
import Tip.Pretty.TFF
import Tip.Passes
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
            let apps = findApps (thy_funcs th) e
            let mFuncs = map (\id -> find ((==) id . func_name) (thy_funcs th)) (map snd apps)
            if Nothing `elem` mFuncs then
                fail "Could not find function"
                else do
                    let funcs = catMaybes mFuncs
                    fIds <- freshIds (map fst apps)
                    sequence $ zipWith (createProperty e) fIds funcs


--betterTest :: (PrettyVar a, Name a) => Theory a -> Expr a -> [(Property a , [[Expr a]])]
--betterTest th e =  

applicativeInduction :: (PrettyVar a, Name a) => [Int] -> Theory a -> Fresh ([Theory a])
applicativeInduction (l:ls) theory = do
    let         expr = fm_body . head . fst . theoryGoals $ theory
    let         newTheory = deleteConjecture 0 theory
    propExpr    <- test newTheory expr >>= \p -> zip p <$> mapM createApps p
    let         prop = fst $ propExpr !! l
    let         ps = Formula Assert [] [] $ propBody prop
    let         varDefs = map (\g -> Signature (gbl_name g) [] (gbl_type g)) (propGlobals prop)
    let         sigs = Signature (gbl_name (propName prop)) [] (gbl_type (propName prop))     
    let         goals = Formula Prove [] [] (Gbl (propName prop) :@: (map (\g -> Gbl g :@: [])) (propGlobals prop))
    let         nTheory = newTheory{thy_asserts = ps : (thy_asserts newTheory) ++ [goals], thy_sigs = sigs : varDefs ++ (thy_sigs newTheory)}
    return $ map (\ props -> nTheory{thy_asserts = (exprs props) ++ thy_asserts nTheory}) (snd (propExpr !! l))
        where exprs ps = map (Formula Assert [] [] ) $ ps
