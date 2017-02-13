module Tip.Passes.Funny where

import Tip.Types
import Tip.Parser
import Tip.Pretty.TFF
import Control.Monad
import Data.Either

{-lookupFunApp :: Formula Id -> IO ()
lookupFunApp f = putStrLn $ show $ findApp (fm_body f) 1 
-}
findApps :: [Function Id] -> Expr Id -> [Expr Id]
findApps fs = findApps' 
    where
        findApps' expr@(Gbl a :@: ts) 
            | isFunc (gbl_name a) fs    = expr:concatMap findApps' ts
            | otherwise                 =  concatMap findApps' ts
        findApps' (_ :@: ts) = concatMap findApps' ts 
        findApps' (Quant _ _ _ e) = findApps' e  
        findApps' (Lcl _) = []
        findApps'  _ = []


isFunc :: Id -> [Function Id] -> Bool
isFunc i = or . map ((==) i . func_name)

-- convert first Expr to a funny property over second Expr 
{- funProp :: Expr Id -> Expr Id -> Expr Id
funProp formula app = renameVar app

renameVar :: Expr Id -> [Expr id]
renameVar expr@(Gbl a :@: ts) = map () ts
    where 
        rName (Lcl a) _ = Lcl a
        rName _ i = Lcl $ Local $ Id ("var" ++ (show i)) i Nothing  -}
