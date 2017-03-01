module Tip.Funny.Application where
import Control.Monad
import Data.List
import Tip.Types
import Tip.Parser
import Tip.Fresh
import Tip.Core
import Tip.Funny.Utils
import Tip.Mod
import Tip.Pretty
import Tip.Pretty.SMT
import Tip.Funny.Property

createApps :: (PrettyVar a, Name a) => Property a -> Fresh [Expr a]
createApps p = 
    let 
        f = propFunc p
        fBody = func_body f
        fArgs = map Lcl $ func_args f
        glbs = map (\g -> Gbl g :@: []) $ propGlobals p
        expr = propBody p
    in do
        renamedExpr <- updateRef' (zip fArgs glbs) fBody
        outp <- mExpr [] p renamedExpr     
        --fail $ show $ ((ppExpr  renamedExpr) : (map (ppExpr) outp)) ++ (map (\p' -> ppExpr (Gbl p' :@: [])) (propGlobals p))
        return outp


mExpr :: Name a => [Expr a] -> Property a -> Expr a -> Fresh [Expr a]
mExpr exprs p (Match m cs)        = 
    do
        inM <- mExpr exprs p m
        lhs <- foldM (getCaseReqs m) [] $ cs  
        let rhs = map (\(Case _ e) -> e) $ reverse cs
        allMatches <- zipWithM 
            (\l e -> 
                do 
                    expr <- mExpr (l:exprs) p e
                    return (case expr of 
                        [] -> [ands (l:exprs)]
                        xs -> xs)) lhs rhs
        --fail $ show (map ppExpr $ concat allMatches)
        return (inM ++ (concat allMatches))
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
                        ++ (show $ ppExpr  e)

gblExpr :: (PrettyVar a, Name a) => [Expr a] -> Property a -> Expr a -> Fresh (Maybe (Expr a))
gblExpr reqs p (Gbl g :@: ls) 
    | gbl_name g == func_name (propFunc p) = 
        do
            let rhsArgs = ls
            prop <- updateRef' (zip (map Lcl (propInp p)) rhsArgs) (propBody p) 
            let prop' = (ands reqs) /\ prop
            let propArgs = free prop'
            let freeVars = nub $ propArgs 
            return $ Just prop' --(mkQuant Forall freeVars prop')
    | otherwise = return Nothing     

getCaseReqs :: Name a => Expr a -> [Expr a] -> Case a -> Fresh ([Expr a])
getCaseReqs _ cs (Case Default e)           = return $ neg (ors cs) : cs
getCaseReqs m cs (Case (LitPat l) e)        = return $ (m === Builtin (Lit l) :@: []) : cs
getCaseReqs m cs (Case (ConPat gbl args) e) = return $ (m === Gbl (gbl) :@: (map Lcl args)) : cs


{-
matchExpr :: Name a => Property a -> Expr a -> Fresh [[Expr a]]
matchExpr p (Match g cs) = 
    do 
        list <- mapM (\c -> patternExpr p g c) cs 
        return list
matchExpr _ _ = fail "Must patternmatch!"

patternExpr :: (PrettyVar a, Name a) => Property a -> Expr a -> Case a -> Fresh [Expr a]
patternExpr p g (Case (ConPat gbl args) e) = 
    do 
        
        -- fix l=Gbl (gbl) :@: args
        let expr = g === (Gbl (gbl) :@: (map Lcl args))
        -- Find all arguments which we can assume our property hold for
        let rhsArgs = [ t | 
                (Gbl g :@: t) <- universe e, gbl_name g == func_name (propFunc p) ]
        -- Create properties with correct arguments
        --let props = [ Gbl (propName p) :@: a | a <- rhsArgs]
        props <- sequence [updateRef' (zip (map Lcl (propInp p)) rhs_args) (propBody p) | rhs_args <- rhsArgs ]
        let propArgs = map free props
        -- add pat_args to forall
        return $ mkQuant Exists args expr : zipWith  (\pr ar -> mkQuant Forall (nub $ args++(propQnts p) ++ ar) . (==>) expr $ pr) props propArgs
callExpr _ _ _ = fail "Unsupported pattern "

-}
