{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}
{-# LANGUAGE ViewPatterns      #-}
module Tip.Mod where

import           Tip.Core
import           qualified Data.Generics.Geniplate  as G   
import           Control.Monad
import           Data.List
import           Data.Maybe
import           Text.PrettyPrint
import           Tip.Fresh
import           Tip.Parser
import           Tip.Passes
import           Tip.Pretty
import           Tip.Pretty.TFF
import           Tip.Rename
import           Tip.Types

-- pretty print a formula in tff format
ppFormula' :: (Ord a, PrettyVar a) => Formula a -> Doc
ppFormula' form =
    case fm_role form of
        Prove  -> clause "goal" "conjecture" body
        Assert -> clause name "axiom" body
    where   name = fromMaybe "axiom" $ join $ lookup "name" $ fm_attrs form
            body = ppExpr 0 (tffify (fm_body form))

-- pretty print a theory in tff format
ppTheory' :: (Ord a, PrettyVar a) => Theory a -> Doc
ppTheory' (renameAvoiding [] (filter validTFFChar) . tffvarify -> Theory{..})
  = vcat
     (map ppSort thy_sorts ++
      map ppUninterp thy_sigs ++
      map ppFormula' thy_asserts)

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


