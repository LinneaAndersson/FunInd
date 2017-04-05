module Tip.Funny.Application where

import           Control.Monad      (foldM, zipWithM)

import           Tip.Core           (ands, neg, ors, (/\), (===), bool, exprType, applyTypeInExpr, applyPolyType)
import           Tip.Fresh          (Fresh, Name)
import           Tip.Funny.Property (Property (..))
import           Tip.Funny.Utils    (updateRef')
import qualified Tip.Pretty.SMT as SMT  (ppExpr)
import           Tip.Types          
import           Tip.Mod             (universe,ppEType, ppType, ppPType, pp)
import           Data.List           (nub)

import           Debug.Trace         (traceM)

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
        mEx<-mExpr [] p renamedExpr
        traceM $ "renExpr: " ++ (show $ SMT.ppExpr renamedExpr)
--        traceM $ ppEType renamedExpr
        
        --traceM $ show $ SMT.ppExpr fBody
        --traceM $ show $ SMT.ppExpr renamedExpr
        pp (\(a,b) -> "AAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAaa(" ++ (show $ SMT.ppExpr a) ++ "," ++ (show $ SMT.ppExpr b) ++ ")"  ) mEx

        return mEx
        --fail $ show $ ((ppExpr  renamedExpr) : (map (ppExpr) outp)) ++ (map (\p' -> ppExpr (Gbl p' :@: [])) (propGlobals p)


mExpr :: Name a => [(Expr a, Expr a)] -> Property a -> Expr a -> Fresh [(Expr a,Expr a)]
mExpr exprs p (Match m cs)        =
    do
        traceM $ "Match cs: " ++ (ppEType m)
        inM <- mExpr exprs p m
        lhs <- foldM (getCaseReqs m) [] cs
        let rhs = map (\(Case _ e) -> e) $ reverse cs
        allMatches <- zipWithM
            (\l e ->
                do
                    expr <- mExpr (l:exprs) p e
                    (case expr of
                        [] -> do 
                                exx <- foldReqs (l:exprs)
                                traceM $ "foldType reqs: " ++ (show $ map (\(a,b)-> "(" ++ (show $ SMT.ppExpr a) ++ "," ++ (show $ SMT.ppExpr b) ++ ")") (l:exprs))
                                traceM $ "foldType: " ++ (show $ SMT.ppExpr exx)
                                return [(exx, bool True)] 
                        xs -> return xs)) lhs rhs 
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

gblExpr :: Name a => [(Expr a, Expr a)] -> Property a -> Expr a -> Fresh (Maybe (Expr a, Expr a))
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

foldReqs :: Name a => [(Expr a,Expr a)] -> Fresh (Expr a)
foldReqs list =
    let exp = last list
        i   = init list
        ri  = reverse i
    in foldM (\e _ -> foldM (\e' l -> updateRef' [l] e' ) e ri) (uncurry (===) exp) i

getCaseReqs :: Name a => Expr a -> [(Expr a,Expr a)] -> Case a -> Fresh [(Expr a, Expr a)]
getCaseReqs m cs (Case Default e)           = fail "default case " --return $ (m, ors (map (neg . snd) cs)) : cs
getCaseReqs m cs (Case (LitPat l) e)        = return $ (m, Builtin (Lit l) :@: []) : cs
getCaseReqs m cs (Case (ConPat gbl ls) e) =
    case exprType m of
        t@(TyCon l list_types) -> do
            let ex = Gbl gbl :@: map Lcl ls
            let newEx = Gbl (Global (gbl_name gbl) (gbl_type gbl) list_types) :@: map Lcl ls

            traceM $ "applyBefore  : " ++ (show $ SMT.ppExpr ex) 
            traceM $ "applyTypeInEx: " ++ (show $ SMT.ppExpr newEx )
        
            return $ (m, newEx) : cs
        _ -> fail "Can only be TyCon in patternmatchng -> in getCaseReq in src/Tip/Funny/Application.hs!!"
--getCaseReqs m cs (Case (ConPat gbl args) e) = return $ (m, Gbl gbl :@: map Lcl args) : cs


{-
    case exprType m of
        t@(TyCon _ list_types) -> do
            let npt = PolyType (polytype_tvs $ gbl_type gbl) [] t
            let ex = Gbl {-(Global (gbl_name gbl) (gbl_type gbl) [BuiltinType Integer])-}gbl :@: []
            let newEx = applyTypeInExpr (polytype_tvs $ gbl_type gbl) list_types ex      
            traceM $ "applyBefore: " ++ (ppEType ex) 
            traceM $ "applyTypeInEx: " ++ (ppEType newEx )

            return $ (m, newEx) : cs
        _ -> fail "Can only be TyCon in patternmatchng -> in getCaseReq in src/Tip/Funny/Application.hs!!"


et npt = PolyType (polytype_tvs $ gbl_type gbl) (polytype_args $ gbl_type gbl) (polytype_res $ gbl_type gbl)
            return $ (m, Gbl (Global (gbl_name gbl) npt [BuiltinType Integer]) :@: []) : cs
-}
