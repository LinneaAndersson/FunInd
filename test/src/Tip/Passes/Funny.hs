module Tip.Passes.Funny where

import Tip.Mod
import Data.List
import Tip.Core hiding (freshArgs)
import Tip.Fresh
import Tip.Types
import Tip.Parser
import Tip.Pretty
import Tip.Pretty.SMT
import Tip.Passes
import Control.Monad
import Data.Either
import Tip.Funny.Property
import Tip.Funny.Utils
import Tip.Funny.Application
import Data.Maybe


--Returns The "sub"-properties of the property
createProperties :: (PrettyVar a, Name a) =>  Theory a -> Expr a -> Fresh [Property a]
createProperties th e = es --freshFrom es th
    where
        es = do
            let apps = findApps (thy_funcs th) e
            let mFuncs = map (\aps -> find ((==) (snd aps) . func_name) (thy_funcs th)) apps
            if Nothing `elem` mFuncs then
                fail "Could not find function"
                else do
                    let funcs = catMaybes mFuncs
                    fIds <- freshIds (map fst apps)
                    zipWithM (createProperty e) fIds funcs

createAsserts ::  (PrettyVar a, Name a) => Theory a -> Expr a -> Fresh [(Property a,[Expr a])]
createAsserts theory expr = createProperties theory expr >>= \p -> zip p <$> mapM createApps p

createGoal ::  (PrettyVar a, Name a) => Property a  -> Fresh (Formula a)
createGoal prop = do
    let        constants = map (\p' -> Gbl p' :@: []) (propGlobals prop ++ propGblBody prop)
    let        lcls = map Lcl (propInp prop ++ propQnts prop)
    propE      <- updateRef'  (zip lcls constants) (propBody prop) 
    return $   Formula Prove [("goal", Nothing)] [] propE --(mkQuant Forall (propQnts prop) propE)

createSignatures :: (PrettyVar a, Name a) => Property a -> [Signature a]
createSignatures prop = map (\g -> Signature (gbl_name g) [] (gbl_type g)) (propGlobals prop)


applicativeInduction :: (PrettyVar a, Name a) => [Int] -> Theory a -> Fresh [Theory a]
applicativeInduction (l:ls) theory' = do
    theory <- head <$> runPasses [TypeSkolemConjecture, Monomorphise False] theory'
    let         goalExpr = fm_body . head . fst . theoryGoals $ theory

    -- Remove the original goal from the theory
    let         newTheory = deleteConjecture 0 theory

    -- 
    propExpr    <-  createAsserts newTheory goalExpr
    --fail $ "propExpr = " ++ (show (map ppExpr $ snd $ propExpr !! l))
    -- update references from locals to constants in the hypotheses
    let         prop = fst $ propExpr !! l
    
    --
    let hyp = snd $ propExpr !! l
    let hyps = map (mkQuant Forall (propQnts prop)) hyp --(updateRef' (zip (map Lcl (propQnts prop)) (map (\gg -> Gbl gg :@: []) (propGblBody prop)))) hyp
    let freeVars = map free hyps
    listFree <- mapM (mapM (\pt -> freshGlobal (PolyType [] [] (lcl_type pt)) [])) freeVars
    hyps' <- mapM (\(fV,lF,hs) -> updateRef' (zip (map Lcl fV) (map (\gg -> Gbl gg :@: []) lF)) hs) $ zip3 freeVars listFree hyps
    let sigsFree = map (\lF -> map (\g -> Signature (gbl_name g) [] (gbl_type g)) (lF ++ propGblBody prop)) listFree

    -- Create the definitions of the constants 
    let         varDefs = map (++ createSignatures prop) sigsFree

    -- create the goal
    goal        <- createGoal prop

    -- update the theory with the new assumptions, signatures and goal
    let         nTheory = newTheory{thy_asserts = thy_asserts newTheory ++ [goal]}
    return $ zipWith (\hs vd -> nTheory{thy_asserts =  thy_asserts nTheory ++ [exprs hs], thy_sigs =  vd ++ thy_sigs newTheory}) hyps' varDefs
        where 
            exprs = Formula Assert [("Assert",Nothing)] []
            
  
