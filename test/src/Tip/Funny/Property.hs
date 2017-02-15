module Tip.Funny.Property where

import Tip.Mod
import Data.List
import Tip.Core hiding (freshArgs)
import Tip.Fresh
import Tip.Types
import Tip.Parser
import Control.Monad
import Tip.Funny.Utils


-- Create one application property 
createPropExpr :: Expr Id -> [(Expr Id, Id)] -> Fresh (Expr Id)
createPropExpr e ids =
    do
        let subst = map (\ei -> (fst ei, createLocal ei)) ids
        
        -- Create prop definition
        prop <- createProp . snd . unzip $ subst 

        -- Update substitution/locals list, create req
        let (locals1, exprs) = partition (isLocal . fst) subst
        newLocals1 <- addReq' locals1 $ map fst exprs
        let locals2 = newLocals1 ++ (map (\ (Lcl l) -> l) $ locals' prop)

        -- Update quantifiers and refresh variables in body
        let rQuant = removeQuant e
        uRefs <- updateRef subst rQuant
        let newLocals2 = [ le | 
                (Lcl le) <- locals' uRefs, not (le `elem` locals2) ]
        let body = mkQuant Forall newLocals2 uRefs

        -- Let substituitions imply body
        req <- addReq subst newLocals1
        let reqImpBody = req ==> body

        -- Let property equal body
        let pEqB = prop === reqImpBody
        -- Add last forall  

        return $ mkQuant Forall (map snd subst) pEqB



addReq :: [(Expr Id, Local Id)] -> [Local Id] -> Fresh (Expr Id)
addReq el newL = 
    do
        let (locals, exps) = partition (isLocal . fst) el
        newEs <- mapM (\(e,i) -> 
            do 
                upRefs <- updateRef locals e
                return (upRefs,i)) exps
        let eqExpr = map (\(a,b)-> Lcl b === a) newEs 
        let andExpr = ands eqExpr
        let fA = mkQuant Forall newL andExpr
        return fA  

addReq' :: [(Expr Id, Local Id)] -> [Expr Id] -> Fresh ([Local Id])
addReq' _  []         = return $ []
addReq' ls (e:es) = 
    do req <- (addReq' (diff ++ ls) es)
       return $ (map snd diff) ++ req 
    where 
        eLoc = [ (e', le)  | e'@(Lcl le)<-locals' e]
        diff = [ (e,i) | (e,i) <- eLoc, not (e `elem` (map fst ls))]  




createProp :: [Local Id] -> Fresh (Expr Id)
createProp ls = (\name -> Gbl (Global name pType []) :@: lcls) <$> gName
    where
        t = BuiltinType Boolean
        gName = (freshNamed "prop")
        gArgs = map lcl_type ls
        pType = PolyType [] gArgs t
        lcls = map Lcl ls



-- convert first Expr to a funny property over second Expr 
freshIds :: [Expr Id] -> Fresh [[(Expr Id, Id)]]-- [Expr Id]
freshIds apps = sequence $ map (p . unzip) (map freshIds apps)
    where  
        freshIds app = freshArgs app --renameVar app
        p (as, bs) = (zip as) <$> (sequence bs)        

freshArgs :: Expr Id -> [(Expr Id, Fresh Id)]
freshArgs expr@(Gbl a :@: ts) = map rName ts
    where 
        rName :: Expr Id -> (Expr Id, Fresh Id)
        rName (Lcl a) = (Lcl a, refresh (lcl_name a))
        rName e = (e, freshNamed "x")
