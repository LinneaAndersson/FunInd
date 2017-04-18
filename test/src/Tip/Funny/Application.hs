module Tip.Funny.Application where

import           Control.Monad      (foldM, zipWithM)

import           Debug.Trace        (traceM)

import           Tip.Core           (ands, neg, ors, (/\), (===), (=/=), bool)
import           Tip.Fresh          (Fresh, Name)
import           Tip.Funny.Property (Property (..))
import           Tip.Funny.Utils    (updateRef')
import qualified Tip.Pretty.SMT as SMT  (ppExpr)
import           Tip.Types          (Builtin (..), Case (..), Expr (..),
                                     Function (..), Global (..), Head (..),
                                     Pattern (..))
import           Tip.Mod             (universe)
import           Data.List           (nub, partition)

type Reqs a = (Expr a, Expr a, Builtin)--(Expr a -> Expr a -> Expr a))

createApps :: Name a => Property a -> Fresh [(Expr a, Expr a)]
createApps p =
    let
        f       = propFunc p
        fBody   = func_body f
        fArgs   = map Lcl $ func_args f
        glbs    = map (\g -> Gbl g :@: []) $ propGlobals p
        expr    = propBody p
    in do
        renamedExpr <- updateRef' (zip fArgs glbs) fBody
        mExpr [] p renamedExpr
        --fail $ show $ ((ppExpr  renamedExpr) : (map (ppExpr) outp)) ++ (map (\p' -> ppExpr (Gbl p' :@: [])) (propGlobals p)


mExpr :: Name a => [Reqs a] -> Property a -> Expr a -> Fresh [(Expr a,Expr a)]
mExpr exprs p (Match m cs)        =
    do
        inM <- mExpr exprs p m
        lhs <- foldM (getCaseReqs m) [] (reverse cs)
        let rhs = map (\(Case _ e) -> e) $ cs
        allMatches <- zipWithM
            (\ls e ->
                do
                    expr <- mExpr (ls ++ exprs) p e
                    (case expr of
                        [] -> do 
                                exx <- foldReqs (ls ++ exprs)
                                return [(exx, bool True)]  --  [(ands (map (uncurry (===)) (l:exprs)), bool True)]
                        xs -> return xs)) lhs rhs --if(containsMatch) then xs else [ands (nub xs)])) lhs rhs
        return (inM ++ concat allMatches)
mExpr exprs p (Builtin g :@: ls)  = concat <$> mapM (mExpr exprs p) ls
mExpr exprs p g@(Gbl g1 :@: ls)      =
    do
        list_exprs <- concat <$> mapM (mExpr exprs p) ls
        o <- gblExpr exprs p g
        case o of
            Nothing -> return list_exprs
            Just e  -> return (e:list_exprs)
mExpr exprs p (Lcl l) = return []
mExpr exprs p (Lam ls e) = mExpr exprs p e
--mExpr exprs p (Let ls lExpr expr) = mExpr
mExpr _ _ e = fail $ "Cannot handle let, letrec or quantifier in expression in: "
                        ++ show (SMT.ppExpr  e)

gblExpr :: Name a => [Reqs a] -> Property a -> Expr a -> Fresh (Maybe (Expr a, Expr a))
gblExpr reqs p (Gbl g :@: rhsArgs)
    | gbl_name g == func_name (propFunc p) =
        do
            prop <- updateRef' (zip (map Lcl (propInp p)) rhsArgs) (propBody p)
            foldedReq <- foldReqs reqs
            --let prop' = foldedReq /\ prop
            --let propArgs = free prop'
            --let freeVars = nub propArgs
            return $ Just (foldedReq, prop) --(mkQuant Forall freeVars prop')
    | otherwise = return Nothing

foldReqs :: Name a => [Reqs a] -> Fresh (Expr a)
foldReqs list'' =
    let (list', distinct) = partition (\(_,_,b) -> b == Equal) list''
        list = map (\(a,b,_) -> (a,b)) list'
        exp = last list
        i   = init list
        ri  = reverse i
    in do
        eq <- foldM (\e _ -> foldM (\e' l -> updateRef' [l] e' ) e ri) (uncurry (===) exp) i
        return . ands $ eq : map (\(a,b,_) -> a =/= b) distinct

getCaseReqs :: Name a => Expr a -> [[Reqs a]] -> Case a -> Fresh [[Reqs a]]
getCaseReqs m cs (Case Default e)           = do
     let cs' = map (\(a,b,_) -> (a, b, Distinct)) (concat cs) : cs
     traceM $ "default case " ++ (show $ SMT.ppExpr e) ++ " not :" ++ (show $ map showReqs (concat cs'))
     return $ map (\(a,b,_) -> (a, b, Distinct)) (concat cs) : cs 
     -- fail $ "default case " ++ (show $ SMT.ppExpr e)
getCaseReqs m cs (Case (LitPat l) e)        = 
    return $ [(m, Builtin (Lit l) :@: [], Equal)] : cs
getCaseReqs m cs (Case (ConPat gbl args) e) = 
    return $ [(m, Gbl gbl :@: map Lcl args, Equal)] : cs

showReqs :: Name a => Reqs a -> String 
showReqs (a,b,Equal) = show $ SMT.ppExpr (a === b)
showReqs (a,b,Distinct) = show $ SMT.ppExpr (a =/= b)
 
