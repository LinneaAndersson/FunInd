module Tip.Passes.Funny where

import Data.List
import Tip.Core hiding (freshArgs)
import Tip.Fresh
import Tip.Types
import Tip.Parser
import Tip.Pretty.TFF
import Control.Monad
import Data.Either


findApps :: [Function Id] -> Expr Id -> [Expr Id]
findApps fs = findApps' 
    where
        findApps' expr@(Gbl a :@: ts) 
            | isFunc (gbl_name a) fs    = expr:concatMap findApps' ts
            | otherwise                 =  concatMap findApps' ts
        findApps' (_ :@: ts) = concatMap findApps' ts 
        findApps' (Quant _ _ _ e) = findApps' e  
        findApps' (Lcl _) = []
        findApps' (Match e cs) = concatMap findApps' $ e:(map case_rhs cs)
        findApps'  _ = []


isFunc :: Id -> [Function Id] -> Bool
isFunc i = or . map ((==) i . func_name)

{-
        e = fm_body formula
        apps = findApps (thy_funcs th) e
        fIds = freshIds th apps-}

--Returns The "sub"-properties of the property
test :: Theory Id -> Expr Id -> [Expr Id]
test th e = freshFrom es th
    where 
        es = do
            fIds <- freshIds $ findApps (thy_funcs th) e
            sequence $ map (createPropExpr e) fIds


-- Create one application property 
createPropExpr :: Expr Id -> [(Expr Id, Id)] -> Fresh (Expr Id)
createPropExpr e ids =
    do
        let subst = map (\ei -> (fst ei, createLocal ei)) ids
        
        -- Create prop definition
        prop <- createProp . snd . unzip $ subst 

        -- Update substitution/locals list, create req
        let (locals1, exprs) = partition (isLocal . fst) subst
        let newLocals1 =  addReq' locals1 $ map fst exprs
        let locals2 = newLocals1 ++ (map (\ (Lcl l) -> l) $ getLocals prop)

        -- Update quantifiers and refresh variables in body
        let rQuant = removeQuant e
        let uRefs = updateRef subst rQuant
        let newLocals2 = [ le | 
                (Lcl le)<-getLocals uRefs, not (le `elem` locals2) ]
        let body = mkQuant Forall newLocals2 uRefs

        -- Let substituitions imply body
        req <- addReq subst newLocals1
        let reqImpBody = req ==> body

        -- Let property equal body
        let pEqB = prop === reqImpBody
        -- Add last forall  

        return $ mkQuant Forall (map snd subst) pEqB

addReq :: [(Expr Id, Local Id)] -> [Local Id] -> Fresh (Expr Id)
addReq el newL = return fA
    where 
        (locals, exps) = partition (isLocal . fst) el
        newEs = map (\(e,i) -> (updateRef locals e,i)) exps
        eqExpr = map (\(a,b)-> Lcl b === a) newEs 
        andExpr = ands eqExpr
        fA = mkQuant Forall newL andExpr  

addReq' :: [(Expr Id, Local Id)] -> [Expr Id] -> [Local Id]
addReq' _  []         = []
addReq' ls (e:es) = (map snd diff) ++ (addReq' (diff ++ ls) es)
    where 
        eLoc = [ (e', le)  | e'@(Lcl le)<-getLocals e]
        diff = [ (e,i) | (e,i) <- eLoc, not (e `elem` (map fst ls))]  

updateRef :: [(Expr Id, Local Id)] -> Expr Id -> Expr Id 
updateRef ls local@(Lcl _)  = 
    case lookup local ls of
        Nothing -> local
        Just l' -> Lcl l' 
updateRef ls gbl@(a :@: ts)    = 
    case lookup gbl ls of
        Nothing -> a :@: (map (updateRef ls) ts)
        Just l' -> Lcl l' 
updateRef ls m@(Match e cs)  = 
    case lookup m ls of
        Nothing -> Match (updateRef ls e) (map (\c -> 
                                c{case_rhs = updateRef ls (case_rhs c)} ) cs)
        Just l' -> Lcl l' 
updateRef ls a             = a

getLocals :: Expr Id -> [Expr Id]
getLocals (Lcl l) = [Lcl l]
getLocals (_ :@: ts) = nub $ concatMap getLocals ts
getLocals (Match e cs) = nub $ concatMap getLocals $ e:(map case_rhs cs)
getLocals  _ = []

isLocal :: Expr Id -> Bool
isLocal (Lcl _) = True
isLocal _       = False

createLocal :: (Expr Id, Id) -> Local Id
createLocal (e, i) = Local i (typeof e) 


createProp :: [Local Id] -> Fresh (Expr Id)
createProp ls = (\name -> Gbl (Global name pType []) :@: lcls) <$> gName
    where
        t = BuiltinType Boolean
        gName = (freshNamed "prop")
        gArgs = map lcl_type ls
        pType = PolyType [] gArgs t
        lcls = map Lcl ls


addForall :: Expr Id -> [(Expr Id, Id)] -> Expr Id
addForall expr fIds = newExp
    where
        locals = map createLocal fIds
        newExp = addQuant locals expr
 
typeof :: Expr Id -> Type Id
typeof (Lcl (Local a b)) = b
typeof (Gbl a :@: _) = polytype_res $ gbl_type a


(=.) :: Expr Id -> Expr Id -> Expr Id 
e1 =. e2 = (Builtin Equal) :@: [e1,e2]

--Add e1 implies e2 
(->.) :: Expr Id -> Expr Id -> Expr Id 
e1 ->. e2 = (Builtin Implies) :@: [e1,e2]

--Add quantifiers to the beginning of the expression
addQuant :: [Local Id] -> Expr Id -> Expr Id
addQuant ls expr = Quant NoInfo Forall ls expr
       
-- Removes all quantifier in the beginning of the expression
removeQuant :: Expr Id -> Expr Id
removeQuant (Quant _ _ _ e) = removeQuant e
removeQuant a = a

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
