module Tip.Funny.Utils where

import Tip.Mod
import Data.List
import Tip.Core hiding (freshArgs)
import Tip.Fresh
import Tip.Types
import Tip.Parser
import Control.Monad


updateRef :: [(Expr Id, Local Id)] -> Expr Id -> Fresh (Expr Id) 
updateRef ls local@(Lcl _)  = return $
    case lookup local ls of
        Nothing -> local
        Just l' -> Lcl l'
updateRef ls gbl@(a :@: ts)    = 
    case lookup gbl ls of
        Nothing -> 
            do
                updated <- mapM (updateRef ls) ts
                return (a :@: updated)
        Just l' -> return $ Lcl l' 
updateRef ls m@(Match e cs)  = 
    case lookup m ls of
        Nothing -> 
            do 
                eUp <- (updateRef ls e) 
                listUp <- mapM (\c -> do 
                    cUp <- updateRef ls (case_rhs c)  
                    return (c{case_rhs = cUp}) ) cs
                return $ Match eUp listUp 
        Just l' -> return $ Lcl l' 
updateRef ls a             = fail "Expression not supported "



isLocal :: Expr Id -> Bool
isLocal (Lcl _) = True
isLocal _       = False

createLocal :: (Expr Id, Id) -> Local Id
createLocal (e, i) = Local i (exprType e) 
       
-- Removes all quantifier in the beginning of the expression
removeQuant :: Expr Id -> Expr Id
removeQuant (Quant _ _ _ e) = removeQuant e
removeQuant a = a

findApps :: [Function Id] -> Expr Id -> Fresh ([Expr Id])
findApps fs e = return $ filter (\(Gbl g :@: _ ) -> isFunc (gbl_name g) fs) (globals' e)


isFunc :: Id -> [Function Id] -> Bool
isFunc i = or . map ((==) i . func_name)
