module Tip.Funny.Utils where

import Tip.Mod
import Data.List
import Data.Maybe
import Tip.Core hiding (freshArgs)
import Tip.Fresh
import Tip.Types
import Tip.Parser
import Tip.Pretty
import Control.Monad



updateRef :: (PrettyVar a, Name a) => [(Expr a, Local a)] -> Expr a -> Fresh (Expr a) 
updateRef = updateRef' . map (\(e,l) -> (e,Lcl l))  

updateRef' :: (PrettyVar a, Name a) => [(Expr a, Expr a)] -> Expr a -> Fresh (Expr a) 
updateRef' ls local@(Lcl _)  = return $ fromMaybe local (lookup local ls)
updateRef' ls gbl@(a :@: ts)    = 
    case lookup gbl ls of
        Nothing -> 
            do
                updated <- mapM (updateRef' ls) ts
                return (a :@: updated)
        Just l' -> return l' 
updateRef' ls m@(Match e cs)  = 
    case lookup m ls of
        Nothing -> 
            do 
                eUp <- updateRef' ls e
                listUp <- mapM (\c -> do 
                    cUp <- updateRef' ls (case_rhs c)  
                    return (c{case_rhs = cUp}) ) cs
                return $ Match eUp listUp 
        Just l' -> return l' 
updateRef' ls (Quant t dn lcl exp) = Quant t dn lcl <$> updateRef' ls exp
updateRef' ls a             = fail "Expression not supported in updateRef'"


isLocal :: (PrettyVar a, Name a) => Expr a -> Bool
isLocal (Lcl _) = True
isLocal _       = False

createLocal :: (PrettyVar a, Name a) => (Expr a, a) -> Local a
createLocal (e, i) = Local i (exprType e) 
       
-- Removes all quantifier in the beginning of the expression
removeQuant :: (PrettyVar a, Name a) => Expr a -> Expr a
removeQuant (Quant _ _ _ e) = removeQuant e
removeQuant a = a

findApps :: (PrettyVar a, Name a) => [Function a] -> Expr a -> [(Expr a, a)]
findApps fs e = funId $ filter (\(Gbl g :@: _ ) -> isFunc (gbl_name g) fs) (globals' e)
    where 
        funId = map (\gbl@(Gbl g :@: ls) -> (gbl, gbl_name g))

isFunc :: (PrettyVar a, Name a) => a -> [Function a] -> Bool
isFunc i = any ((==) i . func_name)
