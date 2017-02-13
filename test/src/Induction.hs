{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses      #-}

module Induction where

import           Control.Monad.State
import           Data.List
import           Data.Maybe
import           Parser.Params
import           Process
import           Prover              hiding (getAxioms)
import           Text.Regex
import           Tip.Formula
import           Tip.Parser
import           Tip.Passes
import           Tip.Types
import           Utils

-- Monad-transformer for induction
newtype InductionT s m a = Induct (StateT s m a)
    deriving (Functor, Applicative, Monad, MonadIO, MonadState s)

data IndState = IndState
  {  params :: Params
  ,  prover :: Prover
  ,  lemmas :: [Lemma]
  ,  ind    :: Maybe Int
  ,  axioms :: [String]
  }

-- Prover/IO instance
type Induction = InductionT IndState IO

data Lemma = Lemma
    { lemmaName :: String
    , hLemmas   :: [String]
    , indVar    :: Maybe Int
    }

getProver :: Induction Prover
getProver = prover <$> get

getAxioms :: Induction [String]
getAxioms = axioms <$> get

getHelpLemmas :: String -> Induction [String]
getHelpLemmas name = lemmas =<< getLemmas
  where
    lemmas ls = case find ((==) name . lemmaName) ls of
                    Nothing -> fail "Lemma not found"
                    Just l  -> return $ hLemmas l

getVar :: Induction (Maybe Int)
getVar = ind <$> get

getLemmas :: Induction [Lemma]
getLemmas = lemmas <$> get

addLemma :: String -> Induction ()
addLemma name = modify (\s ->  s{lemmas = Lemma name (axioms s) (ind s):lemmas s})

-- run the choosen prover on the file given by the filepath
runProver :: FilePath -> Induction String
runProver source = liftIO
                    =<< run_process
                        <$> (name <$> getProver)
                        <*> pure "."
                        <*> fs
    where fs = (++ [source]) <$> (flags <$> getProver)


printResult :: Theory Id -> Induction ()
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
                                        $ "--- Proved using induction on variable: " ++ (show $ getFormulaVar formula i)
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
printStr :: Int -> String -> Induction ()
printStr i s = mwhen ((i <=) <$> (outputLevel . params <$> get))
                (liftIO $ putStrLn s)
