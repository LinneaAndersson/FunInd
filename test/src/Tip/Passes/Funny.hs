module Tip.Passes.Funny where


import           Control.Monad         (zipWithM, when)
import           Data.List             (find, nub)
import           Data.Maybe            (catMaybes, isNothing, fromJust)

import           Tip.Core              hiding (freshArgs)
import           Tip.Fresh             (Fresh, Name)
import           Tip.Funny.Application (createApps)
import           Tip.Funny.Property    as Prop (Property (..), createProperty, freshIds)
import           Tip.Funny.Utils       (findApps, updateRef',quantifyAll)
import           Tip.Mod               (freshGlobal)
import           Tip.Passes            (StandardPass (..),
                                        deleteConjecture, runPasses)
import           Tip.Types             (Expr (..), Formula (..), Quant (..),
                                        Role (..), Signature (..), Theory (..))
import Utils (group)

import Tip.Pretty.SMT


--Returns The "sub"-properties of the property
createProperties :: Name a =>  Theory a -> Expr a -> Fresh [Property a]
createProperties th e = do
    let apps = findApps (thy_funcs th) e
    let mFuncs = map (\aps -> find ((snd aps ==) . func_name) (thy_funcs th)) apps
    if Nothing `elem` mFuncs then
        fail "Could not find function"
        else do
            let funcs = catMaybes mFuncs
            fIds <- Prop.freshIds (map fst apps)
            zipWithM (Prop.createProperty e) fIds funcs


--Returns a "sub"-property of the property specifed by an index
createProperty :: Name a =>  Theory a -> Expr a -> Int -> Fresh (Property a)
createProperty th e i = do
    let app = findApps (thy_funcs th) e !! i
    let func = find ((snd app ==) . func_name) (thy_funcs th)
    if isNothing func then
        fail "Could not find function"
        else do
            fIds <- Prop.freshIds [fst app]
            Prop.createProperty e (head fIds) (fromJust func)


-- Create sub properties and their hypotheses
createAsserts :: Name a => Theory a -> Expr a -> Fresh [(Property a,[(Expr a, Expr a)])]
createAsserts theory expr = createProperties theory expr >>= \p -> zip p <$> mapM createApps p

-- Returns the goal of the property as a formula
createGoal :: Name a => Property a  -> Fresh (Formula a)
createGoal prop = do
    let        constants = map (\p' -> Gbl p' :@: []) (propGlobals prop ++ propGblBody prop)
    let        lcls = map Lcl (propInp prop ++ propQnts prop)
    propE      <- updateRef'  (zip lcls constants) (propBody prop)
    return $   Formula Prove [("goal", Nothing)] [] $ quantifyAll propE

-- Returns signatures for the input arguments of the application we are
-- making induction over
createSignatures :: Name a => Property a -> [Signature a]
createSignatures prop = map (\g -> Signature (gbl_name g) [] (gbl_type g)) (propGlobals prop)


applicativeInduction :: Name a => Bool -> Bool -> [Int] -> Theory a -> Fresh [Theory a]
applicativeInduction _ _      []      _ = fail "No induction indices"
applicativeInduction mono split  (l:ls)  theory' = do
    let passes = (if mono then [TypeSkolemConjecture, Monomorphise False] else []) ++ [LetLift]
    theory <- head <$> runPasses passes theory'    

    -- Get the goal expression from the theory
    let         goalExpr = fm_body . head . fst . theoryGoals $ theory

    -- Remove the original goal from the theory
    let         newTheory = deleteConjecture 0 theory

    --Create all application properties and the goals for each of its "pattern matching cases"
-- TODO : only make one property!!!  
    propExpr    <-  createAsserts newTheory goalExpr

    -- update references from locals to constants in the hypotheses
    let         prop = fst $ propExpr !! l

    -- Create new theories
    if split 
        then applicativeSplit   (snd $ propExpr !! l) prop newTheory         
        else applicativeNoSplit (snd $ propExpr !! l) prop newTheory 
   

applicativeNoSplit :: Name a => [(Expr a, Expr a)] -> Property a -> Theory a -> Fresh [Theory a]
applicativeNoSplit hyp prop theory = do

    -- collect all hypotheses for each pattern matching case
    let collectedExprs = group hyp 
   
    -- List the free variables in the pattern matching cases (global variables)
    let freeVars = concatMap free (map fst collectedExprs)

    --Create new globals for all the free variables
    listFree <- mapM  createFreshGlobal freeVars

    -- update the expression with the new globals
    let lcls = (map Lcl freeVars)
    let gbls = (map (\gg -> Gbl gg :@: []) listFree)
    hypExprs <- mapM (\(req,exprs) -> do
                        lhs <- updateRef' (zip lcls gbls) req
                        rhs <- mapM (updateRef' (zip lcls gbls)) exprs
                        return (lhs,rhs)
                     ) collectedExprs

    -- add quantifier for each hypothesis
    let colQuant = map (\(req,exs) -> ands $ req:map quantifyAll exs) hypExprs

    -- create one hypothesis consisting of all possible pattern matching cases
    let hypExpr' = ors $ colQuant
    
    -- create signatures for all new global variables
    let sigsFree =  map createSig (listFree ++ propGblBody prop)

    -- all the global signatures
    let varDefs = createSignatures prop ++ sigsFree

    -- create the goal
    goal <- createGoal prop

    -- update the theory with the new assumptions, signatures and goal
    return $ [theory{thy_asserts =  [exprs hypExpr'] ++ thy_asserts theory ++ [goal], 
                     thy_sigs    =  varDefs ++ thy_sigs theory}]

        where
            exprs = Formula Assert [("Assert", Nothing)] []
            createFreshGlobal = (\pt -> freshGlobal (PolyType [] [] (lcl_type pt)) [])
            createSig = (\g -> Signature (gbl_name g) [] (gbl_type g))


applicativeSplit :: Name a => [(Expr a, Expr a)] -> Property a -> Theory a -> Fresh [Theory a]
applicativeSplit hyp prop theory = do 

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
    let sigsFree = map (\lF -> map createSig (lF ++ propGblBody prop)) listFree

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


