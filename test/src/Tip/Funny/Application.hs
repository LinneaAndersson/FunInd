module Tip.Funny.Application where

import Tip.Types
import Tip.Parser
import Tip.Fresh
import Tip.Core
import Tip.Funny.Utils
import Tip.Mod
import Tip.Pretty
import Tip.Funny.Property

createApps :: (PrettyVar a, Name a) => Property a -> Fresh([Expr a])
createApps p = 
    let 
        f = propFunc p
        fBody = func_body f
        fArgs = map Lcl $ func_args f
        lcls = propLocals p
        expr = propBody p
    in do
        renamedExpr <- updateRef (zip fArgs lcls) fBody     
        matchExpr p renamedExpr


matchExpr :: (PrettyVar a, Name a) => Property a -> Expr a -> Fresh ([Expr a]) 
matchExpr p (Match (Lcl l) cs) = 
    do 
        list <- mapM (\c -> patternExpr p l c) cs 
        return $ concat list
matchExpr _ _ = fail "Must patternmatch!"

patternExpr :: (PrettyVar a, Name a) => Property a -> Local a -> Case a -> Fresh ([Expr a])
patternExpr p l (Case (ConPat gbl args) e) = 
    do 
        
        -- fix l=Gbl (gbl) :@: args
        let expr = (Lcl l) === (Gbl (gbl) :@: (map Lcl args))
        -- Find all arguments which we can assume our property hold for
        let rhsArgs = [ t | 
                (Gbl g :@: t) <- universe e, gbl_name g == func_name (propFunc p) ]
        -- Create properties with correct arguments
        let props = [ Gbl (propName p) :@: a | a <- rhsArgs]
        -- add pat_args to forall
        return $ map  (mkQuant Forall args . (==>) expr) props
callExpr _ _ _ = fail "Unsupported pattern "
