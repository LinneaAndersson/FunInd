module Tip.Funny.Utils where

import           Data.Maybe (fromMaybe)

import           Tip.Core   (exprType)
import           Tip.Fresh  (Fresh, Name)
import           Tip.Mod    (globals')
import           Tip.Types  (Case (..), Expr (..), Function (..), Global (..),
                             Head (..), Local (..))

import          Tip.Pretty.SMT

updateRef :: Name a => [(Expr a, Local a)] -> Expr a -> Fresh (Expr a)
updateRef = updateRef' . map (\(e,l) -> (e,Lcl l))

updateRef' :: Name a => [(Expr a, Expr a)] -> Expr a -> Fresh (Expr a)
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
updateRef' ls (Lam lcls expr)   = Lam lcls <$> (updateRef' ls expr)
updateRef' ls a                 = fail $ "Expression not supported in updateRef' : " ++ (show $ ppExpr a) 


isLocal :: Name a => Expr a -> Bool
isLocal (Lcl _) = True
isLocal _       = False

createLocal :: Name a => (Expr a, a) -> Local a
createLocal (e, i) = Local i (exprType e)

-- Removes all quantifier in the beginning of the expression
removeQuant :: Name a => Expr a -> Expr a
removeQuant (Quant _ _ _ e) = removeQuant e
removeQuant a               = a

findApps :: Name a => [Function a] -> Expr a -> [(Expr a, a)]
findApps fs e = funId $ filter (\(Gbl g :@: _ ) -> isFunc (gbl_name g) fs) (globals' e)
    where
        funId = map (\gbl@(Gbl g :@: ls) -> (gbl, gbl_name g))

isFunc :: Name a => a -> [Function a] -> Bool
isFunc i = any ((==) i . func_name)
