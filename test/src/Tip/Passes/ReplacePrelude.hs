module Tip.Passes.ReplacePrelude where

import           Control.Monad         (zipWithM, when)
import           Data.List             (find, nub,intersect)
import           Data.Maybe            (catMaybes, isNothing, fromJust)

import           Tip.Core              hiding (freshArgs)
import           Tip.Fresh             (Fresh, Name)
import           Tip.Funny.Application (createApps)
import           Tip.Funny.Property    as Prop (Property (..), createProperty, freshIds)
import           Tip.Funny.Utils       (findApps, updateRef', quantifyAll)
import           Tip.Mod               (freshGlobal,universeBi)
import           Tip.Passes            (StandardPass (..),
                                        deleteConjecture, runPasses)
import           Tip.Types             

import           Utils (group)

import           Tip.Pretty.SMT


--Returns The "sub"-properties of the property
replacePrelude :: Name a =>  Theory a -> Fresh (Theory a)
replacePrelude theory = do
    let builtins = (universeBi theory) :: [Builtin]
    theory' <- if (not $ null (intersect [NumLe,NumLt, NumGt, NumGe] builtins)) then
                    replaceComparisons theory
                    else return theory
    return theory'
        


replaceComparisons :: Name a => Theory a -> Fresh (Theory a)
replaceComparisons theory = do
    --def <- getLEDef 
    let types = nub [exprType (head es) | (Builtin a :@: es) <- universeBi theory, a `elem` [NumLe,NumLt, NumGt, NumGe]] 
    props <- concat <$> mapM getLEAsserts types
    let th = replaceComp theory
    return th{thy_asserts=thy_asserts theory++ props} -- ++ [def]}

getLEAsserts :: Name a => Type a -> Fresh [Formula a]
getLEAsserts t = sequence [trans, antisym, tot,refl]
    where 
        trans = do 
                a <- freshLocal t
                b <- freshLocal t
                c <- freshLocal t
                return $ formula $ (a <<= b) /\ (b <<= c) ==> (a <<= c)
        antisym = do 
                a <- freshLocal t
                b <- freshLocal t
                return $ formula $ (a <<= b) /\ (b <<= a) ==> ((Lcl a) === (Lcl b))
        tot = do 
                a <- freshLocal t
                b <- freshLocal t
                return $ formula $ (a <<= b) \/ (b <<= a) 
        refl = do 
                a <- freshLocal t
                return $ formula $ (a <<= a)
{-
getLEDef :: Name a => Fresh (Formula a)
getLEDef = do
    a <- freshLocal intType
    b <- freshLocal intType
    return $ formula $ (Lcl a) .<<= ((Builtin NumAdd) :@: [Lcl a,Lcl b])
  -}      
formula :: Name a => Expr a -> Formula a
formula = Formula Assert [] [] . quantifyAll   

(<<=) :: Name a => Local a -> Local a -> Expr a
a <<= b = (Builtin NumLe) :@: [Lcl a,Lcl b]

(.<<=) :: Name a => Expr a -> Expr a -> Expr a
a .<<= b = (Builtin NumLe) :@: [a, b]


replaceComp :: Name a => Theory a -> Theory a
replaceComp = transformExprIn replaceComp'
    where
        replaceComp' (Builtin NumLt :@: [a,b]) = neg (b .<<= a) 
        replaceComp' (Builtin NumGt :@: [a,b]) = neg (a .<<= b)
        replaceComp' (Builtin NumGe :@: [a,b]) = b .<<= a
        replaceComp' e = e

