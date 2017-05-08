module Tip.Passes.FunnyNew where 



import           Control.Monad         (zipWithM, when)
import           Data.List             (find, nub)
import           Data.Maybe            (catMaybes, isNothing, fromJust)

import           Tip.Core              hiding (freshArgs)
import           Tip.Fresh             (Fresh, Name, freshFrom, runFreshFrom)
import           Tip.Funny.Utils       (findApps, updateRef',quantifyAll)
import           Tip.Mod               (freshGlobal)
import           Tip.Passes            (StandardPass (..),
                                        deleteConjecture, runPasses)
import           Tip.Types             (Expr (..), Formula (..), Quant (..),
                                        Role (..), Signature (..), Theory (..))
import Utils (group)

import           Tip.FunnyNew.Hypothesis
import           Tip.FunnyNew.Property 
import Tip.Pretty.SMT

applicativeInduction :: Name a => Bool -> [Int] -> Theory a -> Fresh [Theory a]
applicativeInduction _      []      _ = fail "No induction indices"
applicativeInduction split  (l:ls)  theory = do
    theory' <- head <$> runPasses [TypeSkolemConjecture,Monomorphise False,LetLift] theory
    
    -- Get the goal expression from the theory
    let goalExpr = fm_body . head . fst . theoryGoals $ theory'

    prop <- createProperty theory' l goalExpr

    hyps <- createHypotheses prop 
    let hypExprs = groupHypotheses hyps

    ------------ SIGNATURES
    let sigsProp = nub $ free $ propGoal prop
    let sigsHyps = nub $ concatMap free hypExprs
    let sigsLcls = nub $ sigsProp ++ sigsHyps
    gbls <- mapM createFreshGlobal sigsLcls
    let sigs = map createSig gbls 

    ----------------- SUBSTITUTION LIST 
    let subst = zip (map Lcl sigsLcls) (map (\g -> Gbl g :@: []) gbls)

    -------------- GOAL
    goalExpr <- updateRef' subst $ propGoal prop
    let assertGoal = Formula Prove [("goal", Nothing)] [] goalExpr
    goalReqs <- updateRef' subst $ propGoalReqs prop 
    let assertGoalReqs = createAss goalReqs

    ----------------- HYPOTHESES
    hypExprs' <- mapM (updateRef' subst) hypExprs
    let hypAsserts = map createAss $ if split then hypExprs' else [ors hypExprs']

    ------------------ UPDATES
    let asserts = map (\a -> [assertGoal,assertGoalReqs] ++ [a]) hypAsserts
    let th_no_goal = deleteConjecture 0 theory'
    let th_asserts  = map  (++ thy_asserts th_no_goal) asserts
    let th_sigs = thy_sigs th_no_goal ++ sigs


    return $ map (\tha -> th_no_goal{thy_asserts= tha, thy_sigs=th_sigs}) th_asserts

        where 
            createFreshGlobal = (\pt -> freshGlobal (PolyType [] [] (lcl_type pt)) [])
            createSig = (\g -> Signature (gbl_name g) [] (gbl_type g))
            createAss = Formula Assert [("Assert", Nothing)] []

    
groupHypotheses :: Name a => [Hypothesis a] -> [Expr a]
groupHypotheses xs = map (\(req,ass) -> ands $ req:ass) cases
    where cases = group $ map (\h -> (req h, assert h)) xs


