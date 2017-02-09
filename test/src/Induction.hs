{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleInstances #-}

module Induction where

import Control.Monad.State
import Data.List
import Data.Maybe
import Prover hiding (getAxioms)
import Text.Regex
import Tip.Formula
import Tip.Types
import Tip.Parser
import Tip.Passes
import Process
import Parser.Params
import Utils

-- The passes needed to convert the theory into tff format (+ skolemiseconjecture)
passes :: Theory Id -> [Theory Id]
passes = freshPass (runPasses
        [ SkolemiseConjecture
          , TypeSkolemConjecture
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

--type Lemma =  (String, ([String], Bool))

data Lemma = Lemma 
    { lemmaName :: String
    , hLemmas :: [String]
    , indVar :: Maybe Int
    }

getProver :: Induction Prover
getProver = prover <$> get

getAxioms :: Induction [String]
getAxioms = axioms <$> get

getHelpLemmas :: String -> Induction [String]
getHelpLemmas name = lemmas =<< getLemmas
  where 
    lemmas ls = case (find ((==) name . lemmaName) ls) of
                    Nothing -> fail "Lemma not found"
                    Just l -> return $ hLemmas l        

getVar :: Induction (Maybe Int)
getVar = ind <$> get
           
getLemmas :: Induction [Lemma]
getLemmas = lemmas <$> get

addLemma :: String -> Induction ()
addLemma name= modify (\s ->  s{lemmas = ((Lemma name (axioms s) (ind s)):lemmas s)})

-- run the choosen prover on the file given by the filepath
runProver :: FilePath -> Induction String
runProver source = liftIO =<< run_process <$> (name <$> getProver) <*> (pure ".") <*> fs
    where fs = (++ [source]) <$> (flags <$> getProver) :: Induction [Flag]


printResult :: Theory Id -> Induction ()
printResult th = 
    do 
        ls <- getLemmas
        let (ind, notInd) = partition isInductive $ reverse ls
        printStr 0 "\n-------------------------------------------------"
        printStr 0 . show =<< (outputLevel . params <$> get)
        printStr 1 "Summary:"
        printStr 1 ""
        printStr 1 "Proved without induction"
        mapM_ putLemma notInd
        printStr 1 ""
        printStr 1 "Proved with induction"
        mapM_ putLemma ind
        -- printStr 0 $ show $ mapM_ name ls
    where putLemma l = do
            let thy_f       = thy_asserts th
            let nameL        = lemmaName l 
            let formula     = lookupFormula nameL thy_f
            let up          = getUserProperty formula
            let oLevel      = (if isNothing up then 2 else 1 )
            printStr oLevel $ (if isNothing up then nameL else fromJust up ) ++  " " ++ (getFormula formula)
            case indVar l of
                Nothing -> return ()
                Just i  -> printStr oLevel $ "--- Proved using variable: " ++ (show i) 
            case hLemmas l of
                [] -> return ()
                ax -> mapM_ (\a -> printStr oLevel (" | " ++ (if oLevel==1 then getFormula (lookupFormula a thy_f)  else a))) $ nub $ filter ((/=) nameL) ax

isInductive :: Lemma -> Bool
isInductive = isJust . indVar

printStr :: Int -> String -> Induction ()
printStr i s = mcase ((i <=) <$> (outputLevel . params <$> get)) (liftIO $ putStrLn s) (return ())
   
