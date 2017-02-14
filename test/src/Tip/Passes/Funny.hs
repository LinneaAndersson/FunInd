module Tip.Passes.Funny where

import Data.List
import Tip.Core hiding (freshArgs)
import Tip.Fresh
import Tip.Types
import Tip.Parser
import Tip.Pretty.TFF
import Control.Monad
import Data.Either


{-lookupFunApp :: Formula Id -> IO ()
lookupFunApp f = putStrLn $ show $ findApp (fm_body f) 1 
-}

printApps ::  [Function Id] -> Expr Id -> IO ()
printApps fs as = case head $ findApps' as of 
        (Gbl a :@: ts) -> do 
                putStrLn $ show $ gbl_args a
                putStrLn $ show $ gbl_type a
        _ -> return () 
    where
        findApps' expr@(Gbl a :@: ts) 
            | isFunc (gbl_name a) fs    = expr:concatMap findApps' ts
            | otherwise                 =  concatMap findApps' ts
        findApps' (_ :@: ts) = concatMap findApps' ts 
        findApps' (Quant _ _ _ e) = findApps' e  
        findApps' (Lcl _) = []
        findApps'  _ = []


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

test :: Theory Id -> Expr Id -> [Expr Id]
test th e = freshFrom es th
    where
        fIds = freshIds $ 
                findApps (thy_funcs th) e :: Fresh [[(Expr Id, Id)]]
        subst = map (map (\ei -> (fst ei , createLocal ei))) <$> fIds
        prop = do
                lcls <- map (snd . unzip) <$> subst
                sequence $ map createProp lcls
        rQuant = removeQuant e
        faBody = map (\list -> (addReq list) ==> rQuant) <$> subst
        eq = zipWith (===) <$> prop <*> faBody
        es = map (\(p,i) -> addForall p i) <$> (zip <$> eq <*> fIds)

addReq :: [(Expr Id, Local Id)] -> Expr Id
addReq el = fA
    where 
        (locals, exps) = partition (isLocal . fst) el
        newL = (addReq' locals exps)
        newEs = map (\(e,i) -> (updateRef locals e,i)) exps
        eqExpr = map (\(a,b)-> Lcl b === a) newEs 
        andExpr = ands eqExpr
        fA = mkQuant Forall newL andExpr  

addReq' :: [(Expr Id, Local Id)] -> [(Expr Id, Local Id)] -> [Local Id]
addReq' _  []         = []
addReq' ls ((e,l):es) = (map snd diff) ++ (addReq' (diff ++ ls) es)
    where 
        eLoc = [ (e', le)  | e'@(Lcl le)<-getLocals e]
        diff = [ (e,i) | (e,i) <- eLoc, not (e `elem` (map fst ls))]  

updateRef :: [(Expr Id, Local Id)] -> Expr Id -> Expr Id 
updateRef ls local@(Lcl _)  = 
    case lookup local ls of
        Nothing -> local
        Just l' -> Lcl l' 
updateRef ls (a :@: ts)    = a :@: (map (updateRef ls) ts)
updateRef ls (Match e cs)  = Match (updateRef ls e) (map (\c -> 
                                c{case_rhs = updateRef ls (case_rhs c)} ) cs)
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
