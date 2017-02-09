{-# LANGUAGE RecordWildCards, OverloadedStrings, PatternGuards, ViewPatterns #-}
module Tip.Mod where

import Control.Monad
import Data.Maybe
import Data.List
import Text.PrettyPrint
import Tip.Parser
import Tip.Types
import Tip.Pretty
import Tip.Pretty.TFF
import Tip.Rename


ppFormula' :: (Ord a, PrettyVar a) => Formula a -> Doc
ppFormula' form =
    case fm_role form of
        Prove -> clause "goal" "conjecture" body
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
renameLemmas th = th{thy_asserts = new_asserts}
    where 
        lookUpSource s = (join (lookup "source" (fm_attrs s))) 
        addName (f, i) =    
            if (lookup "speculated-lemma" (fm_attrs f)) /= Nothing then f{fm_attrs = (("name", Just $ "lemma"++(show i)):(fm_attrs f))} 
                else 
                    if (lookUpSource f) /= Nothing then f{fm_attrs = (("name", Just $ "lemma"++(show i)):(fm_attrs f))} 
                        else f
        new_asserts = map (addName) $ zip (thy_asserts th) [0..]
