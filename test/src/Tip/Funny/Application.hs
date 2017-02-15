module Tip.Funny.Application where

import Tip.Types
import Tip.Parser
import Tip.Fresh
import Tip.Core
import Tip.Funny.Utils

createApps :: [Function Id]-> Function Id -> Fresh([Expr Id])
createApps f = undefined --caseExpr


matchExpr :: [Function Id] -> Local Id -> Expr Id -> Fresh ([Expr Id]) 
matchExpr fs l (Match e cs) = 
    do 
        list <- mapM (\c -> patternExpr fs l c) cs 
        return $ concat list
matchExpr _ _ _ = fail "Must patternmatch!"

patternExpr :: [Function Id] -> Local Id -> Case Id -> Fresh ([Expr Id])
patternExpr fs l (Case (ConPat gbl args) e) = 
    do 
        
        -- fix l=Gbl (gbl) :@: args
        let expr = (Lcl l) === (Gbl (gbl) :@: (map Lcl args))
        -- Find all applications in e
        apps <- findApps fs e
        -- add pat_args to forall
        return $ map  (mkQuant Forall args . (==>) expr) apps  
callExpr _ _ _ = fail "Unsupported pattern "
