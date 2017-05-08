module Tip.FunnyNew.Hypothesis where

import           Control.Monad   (when)
import           Data.List       (partition,nub)

import           Tip.Core        (free, freshLocal, ands, (===), (==>), exprType, bool, ors, neg,(/\),mkQuant)
import           Tip.Fresh       (Fresh, Name, freshNamed, refresh)
import           Tip.Funny.Utils (findFuncApps, createLocal, isLocal, removeQuant, updateRef', updateRef)
import           Tip.Mod         (freshGlobal, locals')
import           Tip.Types       
import           Tip.FunnyNew.Property

type Req a = Expr a 

data Hypothesis a = Hyp 
    { req :: Req a
    , assert :: Expr a
    }

createHypotheses :: Name a => Property a -> Fresh [Hypothesis a]
createHypotheses prop = do
    let func = propFunc prop
    let repVars = zip ( map Lcl $ func_args func) $ map fst (propArgs prop)
    funcBody <- updateRef' repVars $ func_body func
    getAppReqs prop [] funcBody 

-- Get all the hypothesis (depending on function f)
getAppReqs :: Name a => Property a -> [Req a] -> Expr a -> Fresh [Hypothesis a]
getAppReqs _ _ (Lcl l)              = return []
getAppReqs p rs (Lam ls e)          = getAppReqs p rs e
getAppReqs p rs (Builtin g :@: es) = concat <$> mapM (getAppReqs p rs) es
getAppReqs p rs (Match m cs)        = 
    do
        matchHyps   <- getAppReqs p rs m
        let cases   = getMatchReqs m cs 
        caseHyps    <- 
            concat <$> mapM 
                (\(r,e) -> do
                    hyps <- getAppReqs p (r:rs) e 
                    case hyps of
                        [] -> return [Hyp (ands (r:rs)) (bool True)]
                        xs -> return xs
                ) cases
        return $ matchHyps ++ caseHyps
getAppReqs p rs g@(Gbl g1 :@: es) =
    do
        esHyps <- concat <$> mapM (getAppReqs p rs) es
        if gbl_name g1 == func_name (propFunc p) 
            then getHypAssert p es >>= \assert -> return $ (Hyp (ands rs) assert) : esHyps
            else return esHyps
            
    
getHypAssert :: Name a => Property a -> [Expr a] -> Fresh (Expr a)
getHypAssert p es = 
    let
        {- assume we would like to prove "ordered xs = ordered (insert x xs)"
         by induction over the function application "ordered" on argument "(insert x xs)"
         a = [insert x xs] 
         [b] is references to a (in this case "b = insert x xs")

         We want to create free variables for "x" and "xs", and update "a" with the 
         actual argument in the function application, that is "es" -}
        (a,b) = unzip (propArgs p) 
        argsFree  = concatMap free a -- free variables in the arguments
        
        -- free variables in the whole property expression
        propFree = filter (\l -> not $ l `elem` b) $ free (propBody p)

        -- all free variables to update ("x" and "xs" in the example)
        freeVars = nub $ argsFree ++ propFree 
    in do
        lcls <- mapM (freshLocal . lcl_type) freeVars
        let vars = map Lcl lcls

        {- update the argument expressions (in example "insert x xs" -> "insert a as") 
            to create requirements -}
        let zippedVars = zip (map Lcl freeVars) vars
        updatedArgs <- mapM (updateRef' zippedVars) a

        {- update the property body -}
        let zippedArgs = zip (map Lcl b) es
        updatedBody <- updateRef' (zippedVars++zippedArgs) $ propBody p

        {- sum everything together, in the example:
           es[0] === insert a as && ordered as === ordered es[0]-}
        let updatedReqs = ands $ zipWith (===) es updatedArgs
        return $ mkQuant Forall lcls $ updatedReqs ==> updatedBody
         

-- get all match cases 
getMatchReqs :: Name a => Expr a -> [Case a] -> [(Req a, Expr a)]
getMatchReqs expr cs = 
    case (partition isDefault cs) of
        ([], cs')  -> map (getMatchReq expr) cs' 
        ([d], cs') -> getDefaultMatchReq expr d cs'

-- Check if it is a default case
isDefault :: Case a -> Bool
isDefault (Case Default _)    = True
isDefault _                   = False

-- Get the different pattern requirements (not default)
getMatchReq :: Expr a -> Case a -> (Req a, Expr a)
getMatchReq expr (Case (LitPat l)        e) = (expr === Builtin (Lit l) :@: [],e)
getMatchReq expr (Case (ConPat gbl args) e) = (expr === Gbl gbl :@: map Lcl args,e)
--getMatchReq _ = fail "cannot be default case in getMatchReq"

-- get the default case requirements
getDefaultMatchReq :: Expr a -> Case a -> [Case a] -> [(Req a, Expr a)]
getDefaultMatchReq expr (Case Default e) cs = (neg $ ors $ map fst notDefault, e) : notDefault
    where 
        notDefault = map (getMatchReq expr) cs

