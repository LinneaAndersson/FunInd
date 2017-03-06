module Tip.Funny.Application where

import           Control.Monad      (foldM, zipWithM)

import           Tip.Core           (ands, neg, ors, (/\), (===))
import           Tip.Fresh          (Fresh, Name)
import           Tip.Funny.Property (Property (..))
import           Tip.Funny.Utils    (updateRef')
import           Tip.Pretty.SMT     (ppExpr)
import           Tip.Types          (Builtin (..), Case (..), Expr (..),
                                     Function (..), Global (..), Head (..),
                                     Pattern (..))

createApps :: Name a => Property a -> Fresh [Expr a]
createApps p =
    let
        f = propFunc p
        fBody = func_body f
        fArgs = map Lcl $ func_args f
        glbs = map (\g -> Gbl g :@: []) $ propGlobals p
        expr = propBody p
    in do
        renamedExpr <- updateRef' (zip fArgs glbs) fBody
        mExpr [] p renamedExpr
        --fail $ show $ ((ppExpr  renamedExpr) : (map (ppExpr) outp)) ++ (map (\p' -> ppExpr (Gbl p' :@: [])) (propGlobals p)


mExpr :: Name a => [(Expr a, Expr a)] -> Property a -> Expr a -> Fresh [Expr a]
mExpr exprs p (Match m cs)        =
    do
        inM <- mExpr exprs p m
        lhs <- foldM (getCaseReqs m) [] cs
        let rhs = map (\(Case _ e) -> e) $ reverse cs
        allMatches <- zipWithM
            (\l e ->
                do
                    expr <- mExpr (l:exprs) p e
                    return (case expr of
                        [] -> [ands (map (uncurry (===)) (l:exprs))]
                        xs -> xs)) lhs rhs
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
mExpr _ _ e = fail $ "Cannot handle lamda, let, letrec or quantifier in expression in: "
                        ++ show (ppExpr  e)

gblExpr :: Name a => [(Expr a, Expr a)] -> Property a -> Expr a -> Fresh (Maybe (Expr a))
gblExpr reqs p (Gbl g :@: rhsArgs)
    | gbl_name g == func_name (propFunc p) =
        do
            prop <- updateRef' (zip (map Lcl (propInp p)) rhsArgs) (propBody p)
            foldedReq <- foldReqs reqs
            --let prop' = foldedReq /\ prop
            --let propArgs = free prop'
            --let freeVars = nub propArgs
            return . Just $ foldedReq /\ prop --(mkQuant Forall freeVars prop')
    | otherwise = return Nothing

foldReqs :: Name a => [(Expr a,Expr a)] -> Fresh (Expr a)
foldReqs list =
    let exp = last list
        i   = init list
        ri  = reverse i
    in foldM (\e _ -> foldM (\e' l -> updateRef' [l] e' ) e ri) (uncurry (===) exp) i


getCaseReqs :: Name a => Expr a -> [(Expr a,Expr a)] -> Case a -> Fresh [(Expr a, Expr a)]
getCaseReqs m cs (Case Default e)           = return $ (m, ors (map (neg . snd) cs)) : cs
getCaseReqs m cs (Case (LitPat l) e)        = return $ (m, Builtin (Lit l) :@: []) : cs
getCaseReqs m cs (Case (ConPat gbl args) e) = return $ (m, Gbl gbl :@: map Lcl args) : cs
