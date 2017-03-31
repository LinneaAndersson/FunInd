module Tip.Formula where

import           Control.Monad         (join)
import           Text.Regex            (mkRegex, subRegex)

import           Tip.Core              (forallView)
import           Tip.Fresh             (Name)
import           Tip.Haskell.Translate (trTheory)
import           Tip.Pretty            (PrettyVar (..), pp)
import qualified Tip.Pretty.SMT        as SMT (ppExpr)
import qualified Tip.Pretty.Haskell    as HS (Mode (..))
import qualified Tip.Pretty.TFF        as TFF (ppExpr)
import           Tip.Types             (Formula (..), Local (..), Theory (..))

-- lookup a fomrula with given name
lookupFormula :: Name a => String -> [Formula a] -> Maybe (Formula a)
lookupFormula s [] = Nothing
lookupFormula s (f:fs)
    | join (lookup "name" (fm_attrs f)) == Just s   = Just f
    | otherwise                                     = lookupFormula s fs

-- Returns the i:th variable if input is just Formula
getFormulaVar :: Name a => Formula a -> Int -> String
getFormulaVar f i = varStr $ lcl_name $ fst (forallView $ fm_body f) !! i

-- Returns the formulas name or "no name" if not found
getFormulaName :: Name a => Formula a -> String
getFormulaName f = 
    case join $ lookup "name" $ fm_attrs f of
        Nothing -> "no name"
        Just a  -> a
 
-- TODO rename function?
-- remove formula from Maytbe AND format it
getFormula :: Name a => Formula a -> String
getFormula f = drop 1 $ dropWhile ('=' /=)  $ subRegex (mkRegex "Tip\\.") formula ""
    where formula = show . pp . trTheory HS.Plain $ Theory [] [] [] [] [f]

-- If the formula is user defined, return its name
getUserProperty :: Name a => Formula a -> Maybe String
getUserProperty f = join $ lookup "source" (fm_attrs f)

-- lookup the name of a formula
getAssertion :: Name a => Formula a -> Maybe String
getAssertion = join . lookup "name" . fm_attrs

-- Stringify the body of a Formula
showFormulaSMT :: (Ord a, PrettyVar a) => Formula a -> String
showFormulaSMT = show . SMT.ppExpr . fm_body
{- concatMap repl splitStr
    where
        str = show $ ppExpr 0 $ fm_body fa
        --splitStr = last (splitOn "\n" str)
        --repl '\\' = []
        --repl '"' = []
        --repl c = [c]
-}
