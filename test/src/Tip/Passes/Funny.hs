module Tip.Passes.Funny where


import           Control.Monad         (zipWithM, when)
import           Data.List             (find, nub)
import           Data.Maybe            (catMaybes, isNothing, fromJust)

import           Debug.Trace           (traceM)

import           Tip.Core              hiding (freshArgs)
import           Tip.Fresh             (Fresh, Name, freshPass)
import           Tip.Funny.Application (createApps, Lemma(..))
import           Tip.Funny.Property    as Prop (Property(..), SubProperty (..), createSubProperty, freshIds, findProperty)
import           Tip.Funny.Utils       (findApps, updateRef',quantifyAll)
import           Tip.Mod               (freshGlobal)
import           Tip.Passes            (StandardPass (..), selectConjecture,
                                        deleteConjecture, runPasses)
import           Tip.Types             (Expr (..), Formula (..), Quant (..),
                                        Role (..), Signature (..), Theory (..))
import           Tip.Formula            (getFormulaName, getFormula)
import Utils (group)

import Tip.Pretty.SMT


-- Returns the property represented by the formula
getProperty :: Name a => Theory a -> Formula a -> Fresh (Property a)
getProperty th f = do 
    subProps <- createSubProperties th (fm_body f)
    return $ Prop (getFormulaName f) subProps      



-- Returns a list with all properties in the theory
getProperties :: Name a => Theory a -> Fresh [Property a]
getProperties th = mapM (getProperty th) (fst $ theoryGoals th) 




--Returns The "sub"-properties of the property represented by the expression
createSubProperties :: Name a =>  Theory a -> Expr a -> Fresh [SubProperty a]
createSubProperties th e = do
    let apps = findApps (thy_funcs th) e
    let mFuncs = map (\aps -> find ((snd aps ==) . func_name) (thy_funcs th)) apps
    if Nothing `elem` mFuncs then
        fail "Could not find function"
        else do
            let funcs = catMaybes mFuncs
            fIds <- Prop.freshIds (map fst apps)
            zipWithM (Prop.createSubProperty e) fIds funcs




-- Create sub properties and their hypotheses of the ith property
createAsserts :: Name a => Theory a -> SubProperty a -> [Property a] -> Fresh (SubProperty a,[(Expr a, Maybe (Lemma a))])
createAsserts theory subProp allProps =  do
    apps <- createApps subProp allProps
    return (subProp, apps)





-- Returns the goal of the SubProperty as a formula
createGoal :: Name a => SubProperty a  -> Fresh (Formula a)
createGoal prop = do
    let        constants = map (\p' -> Gbl p' :@: []) (propGlobals prop ++ propGblQnts prop)
    let        lcls = map Lcl (propInp prop ++ propQnts prop)
    propE      <- updateRef'  (zip lcls constants) (propBody prop)
    return $   Formula Prove [("goal", Nothing)] [] propE 



-- Returns signatures for the input arguments of the application we are
-- making induction over
createSignatures :: Name a => SubProperty a -> [Signature a]
createSignatures prop = map (\g -> Signature (gbl_name g) [] (gbl_type g)) (propGlobals prop)




applicativeInduction :: Name a => Bool -> Bool -> [Int] -> Theory a -> Fresh [Theory a]
applicativeInduction _ _     []      _              = fail "No induction indices"
applicativeInduction mutually split (l:ls)  theory' = do
    let old = head . fst . theoryGoals $ selectConjecture 0 theory'
    
    let theory_mut = if mutually then theory' else selectConjecture 0 theory'
    
    --traceM $ "theory before: " ++ (show $ ppTheory [] theory')    

    theory_all <- head <$> runPasses [{- IntToNat, SortsToNat, -}TypeSkolemConjecture, Monomorphise False, LetLift] theory_mut    

    --traceM $ "theory after: " ++ (show $ ppTheory [] theory_all)     

    allPs <- getProperties theory_all

    -- Get the goal expression from the theory
    let         goalFormula = old

    (ps,subProp) <- case Prop.findProperty goalFormula allPs of
                        Nothing -> fail $ "could not find property among :" ++ (show allPs)
                        Just a  -> if mutually then do
                                                let sp = subProps a
                                                when (length sp <= l ) $ end goalFormula old sp l
                                                return (allPs, sp !! l)
                                               else do
                                                let sp = subProps a
                                                when (length sp <= l ) $ end goalFormula old sp l
                                                return ([a{subProps=[sp !! l]}], sp !! l)

    traceM $ "lengttt : " ++ (show $ map (length . subProps) ps )
    -- Remove all conjectures from the theory
    let theory = selectConjecture 0 theory_all
    let         newTheory = deleteConjecture 0 theory

    --Create all application properties and the goals for each of its "pattern matching cases"
    propExpr    <-  createAsserts newTheory subProp ps

    -- update references from locals to constants in the hypotheses
    let         prop = fst propExpr 

    -- Create new theories
    if split 
        then applicativeSplit   (snd propExpr) prop newTheory        
        else applicativeNoSplit (snd propExpr) prop newTheory 
            where end gf old sps i = 
                    fail $ unlines 
                        ["",
                        "goal = " ++ getFormula gf,
                        "Old = " ++ getFormula old,
                        "length sps = " ++ (show $ length sps),
                        "i = " ++ show i]


applicativeNoSplit :: Name a => [(Expr a, Maybe (Lemma a))] -> SubProperty a -> Theory a -> Fresh [Theory a]
applicativeNoSplit hyp prop theory = do

    -- collect all hypotheses for each pattern matching case
    let collectedExprs = group hyp 
   
    let filteredExprs = filter (not . null . snd) $ map (\(e, ls) -> (e, catMaybes ls)) collectedExprs

    -- List the free variables in the pattern matching cases (global variables)
    let freeVars = concatMap free (map fst collectedExprs)

    --Create new globals for all the free variables
    listFree <- mapM  createFreshGlobal freeVars

    -- update the expression with the new globals
    let lcls = (map Lcl freeVars)
    let gbls = (map (\gg -> Gbl gg :@: []) listFree)
    hypExprs <- mapM (\(req,ls) -> do
                        lhs <- updateRef' (zip lcls gbls) req
                        rhs <- mapM (\l -> do 
                                newBody <- updateRef' (zip lcls gbls) (body l) 
                                return l{body = newBody}) ls
                        return (lhs,rhs)
                     ) filteredExprs

    -- add quantifier for each hypothesis
    let colQuant = concatMap  (\(req, ls) -> 
                                    map (\l -> l{body = req 
                                                    ==> quantifyAll (body l)}
                                        ) ls) hypExprs

    let hypOr = ors $ map fst hypExprs
    --map (\(req,exs) -> ands $ req:map quantifyAll exs) hypExprs

    -- create one hypothesis consisting of all possible pattern matching cases
    let hypExpr' = colQuant
    --ors $ colQuant
    
    -- create signatures for all new global variables
    let sigsFree =  map createSig (listFree ++ propGblQnts prop)

    -- all the global signatures
    let varDefs = createSignatures prop ++ sigsFree

    -- create the goal
    goal <- createGoal prop

    -- update the theory with the new assumptions, signatures and goal
    return $ [theory{thy_asserts =  exprs hypOr : (map exprsName hypExpr') ++ thy_asserts theory ++ [goal], 
                     thy_sigs    =  varDefs ++ thy_sigs theory}]

        where
            exprs = Formula Assert [("Assert", Nothing)] []
            exprsName l = Formula Assert 
                [("Assert", Nothing), ("name", Just $ name l)] [] (body l)
            createFreshGlobal = (\pt -> freshGlobal (PolyType [] [] (lcl_type pt)) [])
            createSig = (\g -> Signature (gbl_name g) [] (gbl_type g))





applicativeSplit :: Name a => [(Expr a, Maybe (Lemma a))] -> SubProperty a -> Theory a -> Fresh [Theory a]
applicativeSplit hyp prop theory = undefined {-do 

    -- collect all hypotheses for each pattern matching case
    let collectedExprs = group hyp 

    -- List the free variables in the pattern matching cases (global variables)
    let freeVars = map free (map fst collectedExprs)

    -- Create new globals for each case
    listFree <- mapM (mapM createFreshGlobal) freeVars

    --Update the hypothesises with new globals
    updExprs <- mapM (\(f,l,(req,exprs)) -> do
                        lhs <- updateRef'' (f,l,req)
                        rhs <- mapM (\e -> updateRef'' (f,l,e)) exprs
                        return (lhs,rhs)
                     ) $ zip3 freeVars listFree collectedExprs

    -- add quantifier for each hypothesis
    let hyps' = map (\(req,exs) -> ands $ req:map quantifyAll exs) updExprs

    --Create signatures for the new globals
    let sigsFree = map (\lF -> map createSig (lF ++ propGblQnts prop)) listFree

    -- all the global signatures
    let varDefs = map (++ createSignatures prop) sigsFree

    -- create the goal
    goal  <- createGoal prop

    -- Add the goal to the theory (Same goal for all cases)
    let nTheory = theory{thy_asserts = thy_asserts theory ++ [goal]}

    -- Create new theories. One for each pattern matching case. 
    -- Add the corresponding hypothesis and signatures 
    return $ zipWith (newTheory nTheory) hyps' varDefs

        where
            exprs = Formula Assert [("Assert", Nothing)] []
            createFreshGlobal = (\pt -> freshGlobal (PolyType [] [] (lcl_type pt)) [])
            createSig = (\g -> Signature (gbl_name g) [] (gbl_type g))
            updateRef'' (lcls,gbls,expr) = updateRef' (zip  (createLcls lcls) (createGbls gbls)) expr
            createLcls lcls = map Lcl lcls
            createGbls gbls = map (\gg -> Gbl gg :@: []) gbls
            newTheory th hypothesis signatures = th{thy_asserts =  thy_asserts th ++ [exprs hypothesis], 
                                                    thy_sigs    =  signatures ++ thy_sigs th}

-}
