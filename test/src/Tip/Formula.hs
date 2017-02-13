module Tip.Formula where

import           Control.Monad
import           Data.List
import           Data.List.Split
import           Data.Maybe
import           Text.Regex
import           Tip.Core              (forallView)
import           Tip.Haskell.Translate
import           Tip.Parser
import           Tip.Pretty
import           Tip.Pretty.Haskell    as HS
import           Tip.Pretty.TFF
import           Tip.Types

-- lookup a fomrula with given name
lookupFormula :: String -> [Formula Id] -> Maybe (Formula Id)
lookupFormula s [] = Nothing
lookupFormula s (f:fs)
    | join (lookup "name" (fm_attrs f)) == Just s   = Just f
    | otherwise                                     = lookupFormula s fs

-- Returns the i:th variable if input is just Formula
getFormulaVar :: Formula Id -> Int -> String
getFormulaVar f i = varStr $ lcl_name $ (fst (forallView $ fm_body f)) !! i

-- TODO rename function?
-- remove formula from Maytbe AND format it
getFormula :: Formula Id -> String
getFormula f = drop 1 $ dropWhile ('=' /=)  $ subRegex (mkRegex "Tip\\.") formula ""
    where formula = show . pp . trTheory HS.Plain $ Theory [] [] [] [] [f]

-- If the formula is user defined, return its name
getUserProperty :: Formula Id -> Maybe String
getUserProperty f = join $ lookup "source" (fm_attrs f)

-- lookup the name of a formula
getAssertion :: Formula Id -> Maybe String
getAssertion = join . lookup "name" . fm_attrs

-- Stringify the body of a Formula
showFormula :: (Ord a, PrettyVar a) => Formula a -> String
showFormula = show . ppExpr 0 . fm_body
{- concatMap repl splitStr
    where
        str = show $ ppExpr 0 $ fm_body fa
        --splitStr = last (splitOn "\n" str)
        --repl '\\' = []
        --repl '"' = []
        --repl c = [c]
-}
