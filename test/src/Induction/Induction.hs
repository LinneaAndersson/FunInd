module Induction.Induction where

import           Control.Monad.State (get, liftIO, modify, when, join)
import           Data.List           (nub, partition)
import           Data.Maybe          (fromJust, fromMaybe, isJust, isNothing)

import           Tip.Core            (forallView, theoryGoals)
import           Tip.Formula         (getFormula, getUserProperty,
                                      lookupFormula)
import           Tip.Fresh           (Name)
import           Tip.Funny.Utils     (findApps)
import           Tip.Passes          (induction)
import           Tip.Passes.Funny    (applicativeInduction)
import           Tip.Types           (Formula (..), Theory (..), Head(..), Expr(..), Local(..))
import           Tip.Pretty.SMT      (ppExpr)
import           Tip.Pretty          (ppVar)

import           Induction.Types     (Induction (..), Lemma (..), TP (..), getInduction,
                                      axioms, getLemmas, getProver, ind, lemmas,
                                      params)
import           Parser.Params       (IndType (..), Params (..))
import           IO.Process             (run_process1)
import           Prover              (Prover (..))
import           Utils               (mwhen)


applicativeInd :: Name a => Bool -> Induction a
applicativeInd b =  Ind (\th0 -> length $ findApps (thy_funcs th0) (fm_body $ head . fst $ theoryGoals th0)) 
                        (applicativeInduction b)
                        withIndex
    where
          withIndex th i formula = "--- Proved with application " 
                                    ++ (show i) ++ ": " 
                                    ++ (withArgs $ findApps (thy_funcs th) (fm_body formula) !! (head i))
          withArgs ((Gbl a :@: ls), gbln) = "'" ++ (show $ ppVar gbln) ++ "' with args " ++ (show $ map (ppExpr) ls) 

structuralInd :: Name a => Induction a
structuralInd = Ind (length . fst . forallView . fm_body . head . fst . theoryGoals) 
                    induction
                    withIndex
    where
          withIndex _ i formula = "--- Proved with variables" ++ ": " ++ (show $ map (vars formula !!) i)
          vars = map (ppVar . lcl_name) . fst . forallView . fm_body

addLemma :: Name a => String -> String -> Maybe String -> TP a ()
addLemma name formula source = modify (\s ->  s{lemmas = Lemma name source (axioms s) (ind s) formula:lemmas s})

-- run the choosen prover on the file given by the filepath
runProver :: Name a => FilePath -> TP a String
runProver source = liftIO =<<
                    run_process1
                        <$> (name <$> getProver)
                        <*> pure "."
                        <*> fs
    where
        fs = do
            defFs <- flags <$> getProver
            time <- (head . timeouts) <$> (params <$> get)
            timeout <- (setTime <$> getProver) <*> pure time
            return $ timeout:defFs ++ [source]


printResult :: Name a => Theory a -> TP a ()
printResult th =
    do  -- fetch and partition proved lemmas by if they were proved with or
        -- without induction
        ls <- getLemmas
        let (ind, notInd) = partition isInductive $ reverse ls

        --printStr 0 . show =<< (outputLevel . params <$> get)
        printStr 1 "== Summary =="
        printStr 1 ""
        printStr 1 $ "= Proved without induction (" ++ show (length notInd) ++ ") ="
        mapM_ putLemma notInd
        printStr 1 ""
        printStr 1 $ "= Proved with induction (" ++ show (length ind) ++ ") ="
        mapM_ putLemma ind
        -- printStr 0 $ show $ mapM_ name ls
        printStr 1 ""
        printStr 1 $ "= Could not prove ("    ++ show (length (fst . theoryGoals $ th)) ++ ") ="
        mapM_ printUnproven (fst . theoryGoals $ th)
    where printUnproven f = do
            let userProp    = getUserProperty f
            let outLevel    = if (isNothing userProp) then 2 else 1
            let nameL       = fromJust $ join $ lookup (if (outLevel==1) then "source" else "name") (fm_attrs f)
            printStr outLevel
                        $ fromMaybe nameL userProp
                            ++  " " ++ getFormula f
          putLemma l =
            let -- all formula's in the theory
                thy_f       = thy_asserts th
                -- name of proven lemma
                nameL       = lemmaName l
                -- formula (body) of lemma
                jFormula     = lookupFormula nameL thy_f
            in
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
                    Just i  -> do
                                strify <- printVar <$> getInduction
                                printStr outLevel 
                                            $ strify th i formula
                                       
                -- Print all auxiliary lemmas used in the proof
                userLevel <- outputLevel <$> (params <$> get)
                mapM_ (\a -> printStr outLevel
                    (" | " ++ (if userLevel==1
                                then do
                                    let hFormula = lookupFormula a thy_f
                                    maybe (fail "error: Could not find formula") getFormula hFormula
                                else 
                                    a))) $
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
    Applicative -> applicativeInd (splitCases p)

nextTimeout :: Name a => TP a Bool
nextTimeout = do
    ps <- params <$> get
    let ts = timeouts ps
    if (length ts <= 1)
        then
            return False
        else
            do
                modify $ \s ->
                    s{ params = ps{ timeouts = drop 1 ts } }
                ti <- (show . head . timeouts) <$> (params <$> get)
                printStr 3 (unlines ["== New Timeout == ", " timeout:" ++ ti])
                return True
