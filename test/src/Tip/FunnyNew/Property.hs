module Tip.FunnyNew.Property where

import           Control.Monad   (when)
import           Data.List       (partition)

import           Tip.Core        (freshLocal, ands, (===), (==>), exprType)
import           Tip.Fresh       (Fresh, Name, freshNamed, refresh)
import           Tip.Funny.Utils (findFuncApps, createLocal, isLocal, removeQuant, updateRef', updateRef)
import           Tip.Mod         (freshGlobal, locals')
import           Tip.Types       

import           Tip.Pretty.SMT
import           Tip.Pretty

type PropArg a = (Expr a, Local a) 

data Name a => Property a = Prop 
    { propArgs      :: [PropArg a]
    , propBody      :: Expr a
    , propFunc      :: Function a
    , propGoal      :: Expr a
    , propGoalReqs  :: Expr a
    }

instance Name a => Show (Property a) where
    show p = show $ map ppArgs (propArgs p) ++ [ppExpr (propBody p), ppVar (func_name (propFunc p)) , ppExpr (propGoal p)]
        where ppArgs (exp,lcl) = ppExpr (Lcl lcl === exp)
            

createProperty :: Name a => Theory a -> Int -> Expr a -> Fresh (Property a) 
createProperty th i exprQuant = 
    do
        let expr = removeQuant exprQuant
        let apps = findFuncApps (thy_funcs th) expr
        when (length apps <= i) $ fail "Index out of bounds in createProperty"
        let (app,func) = apps !! i -- get Function

        let (g :@: args_old) = app
        args <- getArgs args_old -- get PropArg

        -- update all arguments to locals except those that already are locals
        let args_new = map (\(e,l) -> if isLocal e then e else Lcl l) args
        prop <- updateRef' [(g :@: args_old, g :@: args_new)] expr --get property

        (goalReqs,goal) <- createGoal args prop
    
        return $ Prop args prop func goal goalReqs

-- create a fresh local for each expression
getArgs :: Name a => [Expr a] -> Fresh [PropArg a]
getArgs arg_old = do
    lcls <- mapM (freshLocal . exprType) arg_old
    return $ zip arg_old lcls
         

createGoal :: Name a => [PropArg a] -> Expr a -> Fresh (Expr a,Expr a)
createGoal args expr = do
    let replaceVars = filter (not . isLocal . fst) args
    updExpr <- updateRef replaceVars expr
    let reqs = map (\(e,l) -> (e === Lcl l)) replaceVars   
    return (ands reqs , updExpr) 
        
    
    
        
