module Tip.Funny.Property where

import Tip.Mod
import Data.List
import Tip.Core hiding (freshArgs)
import Tip.Fresh
import Tip.Types
import Tip.Parser
import Tip.Pretty
import Control.Monad
import Tip.Funny.Utils
import Tip.Pretty.SMT

data (PrettyVar a, Name a) => Property a = Prop 
    { propInp  :: [Local a]
    , propQnts :: [Local a]
    , propGblBody :: [Global a]
    , propBody :: Expr a
    , propFunc :: Function a
    , propGlobals :: [Global a]
    }
    deriving Show

createProperty :: (PrettyVar a, Name a) => Expr a -> [(Expr a, a)] -> Function a -> Fresh (Property a)
createProperty e ids func = do
    (input, qnts, body) <- createPropExpr e ids
    gbls <- mapM (\pt -> freshGlobal (PolyType [] [] (lcl_type pt)) []) input 
    gblBody <- mapM (\pt -> freshGlobal (PolyType [] [] (lcl_type pt)) []) qnts
    --lcls <- mapM freshLocal $ polytype_args (gbl_type name) 
    --fail $ show $ map (\gg -> ppExpr $ Gbl gg :@: []) gbls ++ ([ppExpr body]) ++ (map (ppExpr . Lcl) qnts)
    return $ Prop input qnts gblBody body func gbls  
    

-- Create one application property 
createPropExpr :: (PrettyVar a, Name a) => Expr a -> [(Expr a, a)] -> Fresh ([Local a], [Local a], Expr a)
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
        let body = uRefs--mkQuant Forall newLocals2 uRefs

        -- Let substituitions imply body
        req <- addReq subst newLocals1
        let reqImpBody = req ==> body

        -- Let property equal body
        let pEqB = reqImpBody
        -- Add last forall  

        -- get global from property        
        --let (Gbl _ :@: ts) = prop        
        
        return ((map snd subst),newLocals1++newLocals2, pEqB)



addReq :: (PrettyVar a, Name a) =>[(Expr a, Local a)] -> [Local a] -> Fresh (Expr a)
addReq el newL = 
    do
        let (locals, exps) = partition (isLocal . fst) el
        newEs <- mapM (\(e,i) -> 
            do 
                upRefs <- updateRef locals e
                return (upRefs,i)) exps
        let eqExpr = map (\(a,b)-> Lcl b === a) newEs 
        let andExpr = ands eqExpr
        let fA = andExpr --mkQuant Forall newL andExpr
        return fA  

addReq' :: (PrettyVar a, Name a) =>[(Expr a, Local a)] -> [Expr a] -> Fresh ([Local a])
addReq' _  []         = return $ []
addReq' ls (e:es) = 
    do req <- (addReq' (diff ++ ls) es)
       return $ (map snd diff) ++ req 
    where 
        eLoc = [ (e', le)  | e'@(Lcl le)<-locals' e]
        diff = [ (e,i) | (e,i) <- eLoc, not (e `elem` (map fst ls))]  




createProp :: (PrettyVar a, Name a) =>[Local a] -> Fresh (Expr a)
createProp ls = (\name -> Gbl (Global name pType []) :@: lcls) <$> gName
    where
        t = BuiltinType Boolean
        gName = (freshNamed "prop")
        gArgs = map lcl_type ls
        pType = PolyType [] gArgs t
        lcls = map Lcl ls



-- convert first Expr to a funny property over second Expr 
freshIds :: (PrettyVar a, Name a) =>[Expr a] -> Fresh [[(Expr a, a)]]-- [Expr a]
freshIds apps = sequence $ map (p . unzip) (map freshIds apps)
    where  
        freshIds app = freshArgs app --renameVar app
        p (as, bs) = (zip as) <$> (sequence bs)        

freshArgs :: (PrettyVar a, Name a) =>Expr a -> [(Expr a, Fresh a)]
freshArgs expr@(Gbl a :@: ts) = map rName ts
    where 
        rName :: (PrettyVar a, Name a) =>Expr a -> (Expr a, Fresh a)
        rName (Lcl a) = (Lcl a, refresh (lcl_name a))
        rName e = (e, freshNamed "x")
