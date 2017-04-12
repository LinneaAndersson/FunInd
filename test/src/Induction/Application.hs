module Induction.Application where

import           Control.Monad.State (when, join, modify, liftIO)

import           Data.List           (partition,nub,(\\), find)
import           Data.Maybe          (fromJust, isNothing, catMaybes)

import           Tip.Core            (forallView, theoryGoals)
import           Tip.Formula         (getFormula, getUserProperty,
                                      lookupFormula)
import           Tip.Fresh           (Name)
import           Tip.Funny.Utils     (findApps)
import           Induction.Types     (Induction (..), Lemma (..), TP (..), getInduction,
                                      axioms, getLemmas, getProver, ind, lemmas,
                                      params)
import qualified Tip.Funny.Property as Prop (Property(..))
import           Tip.Passes.Funny    (applicativeInduction)
import           Tip.Passes          (selectConjecture,provedConjecture)
import           Tip.Types           (Role(..),Formula (..), Theory (..), Head(..), Expr(..), Local(..))
import           Tip.Pretty.SMT      (ppExpr)
import           Tip.Pretty          (ppVar)

import           Utils               (deleteAt)

import           Debug.Trace         (trace, traceM)

applicationInd :: Name a => Bool -> Induction a
applicationInd b =  Ind (\th0 -> length $ findApps (thy_funcs th0) (fm_body $ head . fst $ theoryGoals th0)) 
                        [applicativeInduction False b, applicativeInduction True b]
                        withIndex
                        (return . selectConjecture 0)
                        provedConjApp
    where
          withIndex th i formula = "--- Proved with application " 
                                       ++ (show i) ++ ": " 
                                    ++ (withArgs $ findApps (thy_funcs th) (fm_body formula) !! (head i))
          withArgs ((Gbl a :@: ls), gbln) = "'" ++ (show $ ppVar gbln) ++ "' with args " ++ (show $ map (ppExpr) ls) 


selectConjApp :: Name a => Theory a -> TP a [Theory a]
selectConjApp th = return [th1,th2]
    where 
        th1 = selectConjecture 0 th
        goals = fst $ theoryGoals th
        -- Assume all lemmas are proven (except the one we would like to prove)
        th2 = th1{thy_asserts = thy_asserts th1 ++ (map (\l -> l{fm_role=Assert}) (drop 1 goals))}

provedConjApp :: Name a => Theory a -> TP a (Theory a)
provedConjApp th = do
    (new:ls) <- getLemmas

    let provenLemmas = filter status ls 
    let newLemmas = updateProven $ new{status=False}:ls
          --case canProve new provenLemmas of
            --Nothing ->  
              --  updateProven $ new{status=False}:ls
            --Just a ->  updateProven  $ new{hLemmas=[a]}:ls 
    modify (\s -> s{lemmas=newLemmas})
    let proven' = map lemmaName $ filter status newLemmas
    let proven = proven' \\ (map lemmaName provenLemmas)
    --traceM $ unlines ["before: ",(show (map lemmaName provenLemmas))]
    --traceM $ unlines ["proven': ",(show proven')]
    --traceM $ unlines ["proven: ",(show proven)]
    let (gs,as) = theoryGoals th
    if null proven then do
        --liftIO $ putStrLn "in null proven "
        return th{thy_asserts = tail gs ++ [head gs] ++ as}
        else 
            return th{thy_asserts = (map (\g -> if fname g `elem` proven then g{fm_role = Assert} else g) gs) ++ as}
        where fname = fromJust . join . lookup "name" . fm_attrs

updateProven :: [Lemma] -> [Lemma]
updateProven ls = map update ls
    where 
        upLemmas = addProven ls
        proven1 = filter status upLemmas
        update l = case find (\l' -> lemmaName l == lemmaName l') proven1 of
                    Nothing -> l
                    Just a  -> a


addProven :: [Lemma] -> [Lemma] 
--addProven [] ls = ls
addProven ls = map update proven
    where
        proven = canProveLoop ls
        update l = if not $ status l then
            trace (":) " ++ lemmaName l) $ l{status=True, hLemmas=[head $ hLemmas l]}
            else l

{-
canProve :: Lemma -> [Lemma] -> Maybe (Maybe [Int], [String])
canProve lemma ls = help (hLemmas lemma) 
    where
        provenLemmas = nub $ map (\h -> lemmaName h {- case lemmaSource h of
                                    Nothing -> lemmaName h
                                    Just a -> a -}) (lemma:ls)
        help []                    = Nothing
        help (tp@(indvar, hs):hss) = if and $ map (\l -> l `elem` provenLemmas) (nub hs) then
                                            Just tp 
                                            else help hss 
-}

canProveLoop :: [Lemma] -> [Lemma] 
canProveLoop ls = if length updated == length ls then updated else canProveLoop updated
    where  
        --fakeLemmas = filter (not . status) ls
        updateLemma l = if status l then Just l else updateFakeLemmas l ls 
        updated = catMaybes (map updateLemma ls)
        
        --lemmaNames = map lemmaName ls
        


updateFakeLemmas :: Lemma -> [Lemma] -> Maybe Lemma
updateFakeLemmas lemma ls = if null newHLemmas then Nothing else Just lemma{hLemmas=newHLemmas}
    where 
        oldHLemmas = hLemmas lemma
        lemmaNames = map lemmaName ls
        newHLemmas = filter (\(_, h) -> trace (unlines . nub $ (("== " ++ lemmaName lemma):h)) $ all (\l -> l `elem` lemmaNames) h) oldHLemmas





