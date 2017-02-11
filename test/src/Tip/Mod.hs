{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}
{-# LANGUAGE ViewPatterns      #-}
module Tip.Mod where

import           Control.Monad
import           Data.List
import           Data.Maybe
import           Text.PrettyPrint
import           Tip.Parser
import           Tip.Pretty
import           Tip.Pretty.TFF
import           Tip.Rename
import           Tip.Types


ppFormula' :: (Ord a, PrettyVar a) => Formula a -> Doc
ppFormula' form =
    case fm_role form of
        Prove  -> clause "goal" "conjecture" body
        Assert -> clause name "axiom" body
    where   name = fromMaybe "axiom" $ join $ lookup "name" $ fm_attrs form
            body = ppExpr 0 (tffify (fm_body form))

ppTheory' :: (Ord a,PrettyVar a) => Theory a -> Doc
ppTheory' (renameAvoiding [] (filter validTFFChar) . tffvarify -> Theory{..})
  = vcat
     (map ppSort thy_sorts ++
      map ppUninterp thy_sigs ++
      map ppFormula' thy_asserts)


renameLemmas :: Theory a -> Theory a
renameLemmas th = th{thy_asserts = new_asserts th}
    where
        lookupSource = join . lookup "source" . fm_attrs
        addName (i, f)
            | isJust (lookup "speculated-lemma" (fm_attrs f)) || isJust (lookupSource f) =
                f{fm_attrs = ("name", Just $ "lemma" ++ show i) : fm_attrs f}
            | otherwise = f
        new_asserts = zipWith (curry addName) [0 ..] . thy_asserts
