module Induction.Induction where

import           Control.Monad.State
import           Data.List
import           Data.Maybe
import           Induction.Types
import           Parser.Params
import           Process
import           Prover              hiding (getAxioms)
import           Text.Regex
import           Tip.Core
import           Tip.Passes.Funny
import           Tip.Fresh
import           Tip.Formula
import           Tip.Parser
import           Tip.Passes
import           Tip.Pretty
import           Tip.Types
import           Utils
import           Tip.Funny.Utils


applicativeInd :: Name a => Induction a
applicativeInd =  Ind (\th0 -> length $ findApps (thy_funcs th0) (fm_body $ head . fst $ theoryGoals th0)) applicativeInduction

structuralInd :: Name a => Induction a
structuralInd = Ind (length . fst . forallView . fm_body . head . fst . theoryGoals) induction


addLemma :: Name a => String -> TP a ()
addLemma name = modify (\s ->  s{lemmas = Lemma name (axioms s) (ind s):lemmas s})

-- run the choosen prover on the file given by the filepath
runProver :: Name a => FilePath -> TP a String
runProver source = liftIO
                    =<< run_process
                        <$> (name <$> getProver)
                        <*> pure "."
                        <*> fs
    where
        fs = do
            defFs <- (flags <$> getProver)
            time <- (head . timeouts) <$> (params <$> get)
            timeout <- (setTime <$> getProver) <*> pure time
            return $ timeout:defFs ++ [source]


printResult :: Name a => Theory a -> TP a ()
printResult th =
    do  -- fetch and partition proved lemmas by if they were proved with or
        -- without induction
        ls <- getLemmas
        let (ind, notInd) = partition isInductive $ reverse ls

        printStr 0 "\n-------------------------------------------------"
        --printStr 0 . show =<< (outputLevel . params <$> get)
        printStr 1 "Summary:"
        printStr 1 ""
        printStr 1 "Proved without induction"
        mapM_ putLemma notInd
        printStr 1 ""
        printStr 1 "Proved with induction"
        mapM_ putLemma ind
        -- printStr 0 $ show $ mapM_ name ls
    where putLemma l =
            let -- all formula's in the theory
                thy_f       = thy_asserts th
                -- name of proven lemma
                nameL       = lemmaName l
                -- formula (body) of lemma
                jFormula     = lookupFormula nameL thy_f
            in do
            if isNothing jFormula then fail "error: Could not find formula" else
                let
                    formula     = fromJust jFormula
                    -- whether it is a user defined property
                    userProp    = getUserProperty formula
                    -- the level of verbosity
                    outLevel    = (if isNothing userProp then 2 else 1 )

                in do
                printStr outLevel
                        $ fromMaybe nameL userProp
                            ++  " " ++ getFormula formula
                -- If proved with induction, show which variable was used
                case indVar l of
                    Nothing -> return ()
                    Just i  -> printStr outLevel
                                        $ "--- Proved with index: " ++ (show i) -- ++ (show $ getFormulaVar formula i)
                -- Print all auxiliary lemmas used in the proof
                mapM_ (\a -> printStr outLevel
                    (" | " ++ (if outLevel==1
                                then do
                                    let hFormula = (lookupFormula a thy_f)
                                    if isNothing hFormula then fail "error: Could not find formula" else getFormula (fromJust hFormula)
                                else a))) $
                    nub $ filter (nameL /=) (hLemmas l)

-- check if a lemma was proved using induction
isInductive :: Lemma -> Bool
isInductive = isJust . indVar

-- print if given int is less then the verbosity level
printStr :: Name a => Int -> String -> TP a ()
printStr i s = mwhen ((i <=) <$> (outputLevel . params <$> get))
                (liftIO $ putStrLn s)

getIndType :: Name a => Params -> Induction a
getIndType p = case indType p of
    Structural  -> structuralInd
    Applicative -> applicativeInd
