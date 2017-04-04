{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}
{-# LANGUAGE ViewPatterns      #-}
module Tip.Mod where


import qualified Data.Generics.Geniplate as G   (UniverseBi, universe, universeBi)
import           Control.Monad                  (join)
import           Data.Maybe                     (isJust, fromMaybe)
import           Text.PrettyPrint               (Doc(..), vcat)

import           Tip.Core                       (locals, exprType)
import           Tip.Fresh                      (Name, Fresh(..), fresh)
import           Tip.Passes                     (StandardPass(..), runPasses,
                                                 freshPass)
import           Tip.Pretty                    (PrettyVar, ppVar)
import qualified Tip.Pretty.TFF as TFF         (clause, ppExpr, ppSort,
                                                 ppUninterp, tffify, tffvarify,
                                                 validTFFChar)
import           Tip.Rename                     (renameAvoiding)
import           Tip.Types                      (Head(..), Global(..), Type(..),
                                                 PolyType(..),Expr(..),
                                                 Theory(..), Formula(..),
                                                 Role(..))

import          Debug.Trace (traceM)

-- pretty print a formula in tff format
ppFormulaTFF :: (Ord a, PrettyVar a) => Formula a -> Doc
ppFormulaTFF form =
    case fm_role form of
        Prove  -> TFF.clause "goal" "conjecture" body
        Assert -> TFF.clause name "axiom" body
    where   name = fromMaybe "axiom" $ join $ lookup "name" $ fm_attrs form
            body = TFF.ppExpr 0 (TFF.tffify (fm_body form))

-- pretty print a theory in tff format
ppTheoryTFF :: (Ord a, PrettyVar a) => Theory a -> Doc
ppTheoryTFF (renameAvoiding [] (filter TFF.validTFFChar) . TFF.tffvarify -> Theory{..})
  = vcat
     (map TFF.ppSort thy_sorts ++
      map TFF.ppUninterp thy_sigs ++
      map ppFormulaTFF thy_asserts)

-- rename all speculated-lemmas and source properties with lemm1, lemma2, etc.
renameLemmas :: (PrettyVar a, Name a) => Theory a -> Theory a
renameLemmas th = th{thy_asserts = new_asserts th}
    where
        -- check if formula is a source property
        lookupSource = join . lookup "source" . fm_attrs
        addName (i, f)
            | isJust (lookup "speculated-lemma" (fm_attrs f)) || isJust (lookupSource f) =
                f{fm_attrs = ("name", Just $ "lemma" ++ show i) : fm_attrs f}
            | otherwise = f
        -- zip formulas with new name, if they are speculated or source
        new_asserts = zipWith (curry addName) [0 ..] . thy_asserts

-- The passes needed to convert the theory into tff format
tff :: (PrettyVar a, Name a) => [StandardPass]-> Theory a -> [Theory a]
tff p = freshPass (runPasses $ p ++
        [ TypeSkolemConjecture
          , Monomorphise False
          , LambdaLift
          , AxiomatizeLambdas
          , SimplifyGently
          , CollapseEqual
          , RemoveAliases
          , SimplifyGently
          , Monomorphise False
          , IfToBoolOp
          , CommuteMatch
          , SimplifyGently
          , LetLift
          , SimplifyGently
          , AxiomatizeFuncdefs2
          , SimplifyGently
          , AxiomatizeDatadecls
        ])

globals' :: (PrettyVar a, Name a) => Expr a -> [Expr a]
globals' e = [gbl  | gbl@(Gbl g :@: t) <- universe e]

locals' :: (PrettyVar a, Name a) =>  Expr a -> [Expr a]
locals' = map Lcl . locals

universe :: G.UniverseBi a a => a -> [a]
universe = G.universe

universeBi :: G.UniverseBi a b => a -> [b]
universeBi = G.universeBi

freshGlobal :: (PrettyVar a, Name a) => PolyType a -> [Type a] -> Fresh (Global a)
freshGlobal pt t =
    do
        id <- fresh
        return $ Global id pt t

ppEType :: Name a => Expr a -> String
ppEType = ppType . exprType

ppType :: Name a => Type a -> String
ppType (TyVar a) = "TyVar: " ++ (show $ ppVar a) 
ppType (TyCon a ts) = unlines $ 
        ["TyCon: " ++ (show $ ppVar a)] ++ (map ppType ts)
ppType (BuiltinType _) = "BuiltinType" 
ppType (_ :=>: _) = "Function"

pp :: (Applicative f) => (a -> String) -> [a] -> f ()
pp f = traceM . unlines . map f
