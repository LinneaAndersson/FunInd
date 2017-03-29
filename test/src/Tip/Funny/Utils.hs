module Tip.Funny.Utils where

import           Data.Maybe (fromMaybe)

import           Tip.Core   (exprType)
import           Tip.Fresh  (Fresh, Name, fresh)
import           Tip.Mod    (globals')
import           Tip.Types  (Case (..), Pattern(..), Expr (..), Function (..), Global (..),
                             Head (..), Local (..))

import qualified Tip.Pretty.SMT as SMT

updateRef :: Name a => [(Expr a, Local a)] -> Expr a -> Fresh (Expr a)
updateRef = updateRef' . map (\(e,l) -> (e,Lcl l))

updateRef' :: Name a => [(Expr a, Expr a)] -> Expr a -> Fresh (Expr a)
updateRef' ls local@(Lcl _) = return $ fromMaybe local (lookup local ls)
updateRef' ls gbl@(a :@: ts) =
    case lookup gbl ls of
        Nothing ->
            do
                updated <- mapM (updateRef' ls) ts
                return (a :@: updated)
        Just l' -> return l'
updateRef' ls m@(Match e cs) =
    case lookup m ls of
        Nothing ->
            do
                eUp     <- updateRef' ls e
                listUp  <- mapM (updateCase ls) cs
                return $ Match eUp listUp
        Just l' -> return l'
updateRef' ls (Quant t dn lcls expr) = do
    fVar    <- mapM (\l -> fresh >>= \i -> return $ Local i (lcl_type l)  ) lcls
    expr'   <- updateRef (zip (map Lcl lcls) fVar) expr
    Quant t dn fVar <$> updateRef' ls expr'
updateRef' ls (Lam lcls expr) = do
    fVar    <- mapM (\l -> fresh >>= \i -> return $ Local i (lcl_type l)  ) lcls
    expr'   <- updateRef (zip (map Lcl lcls) fVar) expr
    Lam fVar <$> (updateRef' ls expr')
updateRef' ls (Let lcl eLcl expr) = do
    fVar    <-  fresh >>= \i -> return $ Local i (lcl_type lcl)
    lclExpr <- updateRef [(Lcl lcl,fVar)] expr
    exprU   <- updateRef' ls lclExpr
    eLcLU   <- updateRef' ls eLcl
    return (Let fVar eLcLU exprU)
updateRef' ls a = fail $ "Expression not supported in updateRef' : " ++ (show $ SMT.ppExpr a) 

updateCase :: Name a => [(Expr a, Expr a)] -> Case a -> Fresh (Case a)
updateCase ls (Case (ConPat con lcls) rhs) = do 
    fVar <- mapM (\l -> fresh >>= \i -> return $ Local i (lcl_type l)) lcls
    rh2 <- updateRef (zip (map Lcl lcls) fVar) rhs
    cUp <- updateRef' ls rh2
    return $ Case (ConPat con fVar) cUp
updateCase ls (Case pat rhs) = updateRef' ls rhs >>= return . Case pat  

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
isFunc i = any ((i ==) . func_name)
