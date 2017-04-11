module Tip.Funny.Application where

import           Control.Monad      (foldM, zipWithM)

import           Debug.Trace    

import           Tip.Core           (ands, neg, ors, (/\), (===), bool)
import           Tip.Fresh          (Fresh, Name)
import           Tip.Funny.Property (SubProperty (..), Property(..),functionSubProperties)
import           Tip.Funny.Utils    (updateRef')
import qualified Tip.Pretty.SMT as SMT  (ppExpr)
import           Tip.Types          (Builtin (..), Case (..), Expr (..),
                                     Function (..), Global (..), Head (..),
                                     Pattern (..))
import           Tip.Mod             (universe)
import           Data.List           (nub)

createApps :: Name a => SubProperty a -> [Property a] -> Fresh [(Expr a, Maybe (SubProperty a))]
createApps p ps =
    let
        f       = propFunc p
        fBody   = func_body f
        fArgs   = map Lcl $ func_args f
        glbs    = map (\g -> Gbl g :@: []) $ propGlobals p
        expr    = propBody p
    in do
        renamedExpr <- updateRef' (zip fArgs glbs) fBody
        mExpr ps [] p renamedExpr


mExpr :: Name a => [Property a] -> [(Expr a, Expr a)] -> SubProperty a -> Expr a -> Fresh [(Expr a, Maybe (SubProperty a))]
mExpr ps exprs p (Match m cs)        =
    do
        inM <- mExpr ps exprs p m
        lhs <- foldM (getCaseReqs m) [] cs
        let rhs = map (\(Case _ e) -> e) $ reverse cs
        allMatches <- zipWithM
            (\l e ->
                do
                    expr <- mExpr ps (l:exprs) p e
                    let containsMatch = not $ null [0 | (Match _ _) <- universe e]
                    (case expr of
                        [] -> do 
                                exx <- foldReqs (l:exprs)
                                return [(exx, Nothing)]  --  [(ands (map (uncurry (===)) (l:exprs)), bool True)]
                        xs -> return xs)) lhs rhs --if(containsMatch) then xs else [ands (nub xs)])) lhs rhs
        return (inM ++ concat allMatches)
mExpr ps exprs p (Builtin g :@: ls)  = concat <$> mapM (mExpr ps exprs p) ls
mExpr ps exprs p g@(Gbl g1 :@: ls)   =
    do
        list_exprs <- concat <$> mapM (mExpr ps exprs p) ls
        o <- gblExpr ps exprs g
        return $ o ++ list_exprs
       -- case o of
         --   Nothing -> return list_exprs
           -- Just e  -> return (e:list_exprs)
mExpr ps exprs p (Lcl l) = return []
mExpr ps exprs p (Lam ls e) = mExpr ps exprs p e
--mExpr exprs p (Let ls lExpr expr) = mExpr
mExpr _ _ _ e = fail $ "Cannot handle let, letrec or quantifier in expression in: "
                        ++ show (SMT.ppExpr  e)

gblExpr :: Name a => [Property a] -> [(Expr a, Expr a)] -> Expr a -> Fresh ([(Expr a, Maybe (SubProperty a))])
gblExpr ps reqs (Gbl g :@: rhsArgs) = do
    let hyps = functionSubProperties g ps
    traceM $ "length hyps ::: " ++ (show $ length $ hyps)
    mapM (\p -> do
        prop <- updateRef' (zip (map Lcl (propInp p)) rhsArgs) (propBody p)
        foldedReq <- foldReqs reqs
        return (foldedReq, Just p{propBody=prop}))
        hyps

{-- 
   | gbl_name g == func_name (propFunc p) =
        do
            prop <- updateRef' (zip (map Lcl (propInp p)) rhsArgs) (propBody p)
            foldedReq <- foldReqs reqs
            return $ Just (foldedReq, prop) 
    | otherwise = return Nothing 
--}
foldReqs :: Name a => [(Expr a,Expr a)] -> Fresh (Expr a)
foldReqs list =
    let exp = last list
        i   = init list
        ri  = reverse i
    in foldM (\e _ -> foldM (\e' l -> updateRef' [l] e' ) e ri) (uncurry (===) exp) i

getCaseReqs :: Name a => Expr a -> [(Expr a,Expr a)] -> Case a -> Fresh [(Expr a,Expr a)]
getCaseReqs m cs (Case Default e)           = fail "default case " --return $ (m, ors (map (neg . snd) cs)) : cs
getCaseReqs m cs (Case (LitPat l) e)        = return $ (m, Builtin (Lit l) :@: []) : cs
getCaseReqs m cs (Case (ConPat gbl args) e) = return $ (m, Gbl gbl :@: map Lcl args) : cs
