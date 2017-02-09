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
  {  prover :: Prover 
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


printResult :: [Lemma] -> Theory Id -> IO ()
printResult ls th = 
    do 
        let (ind, notInd) = partition isInductive $ reverse ls
        putStrLn "\n-------------------------------------------------"
        putStrLn "Summary:"
        putStrLn ""
        putStrLn "Proved without induction"
        mapM_ putLemma notInd
        putStrLn ""
        putStrLn "Proved with induction"
        mapM_ putLemma ind
    where putLemma l = do
            let thy_f = thy_asserts th
            let name = lemmaName l 
            let formula = lookupFormula name thy_f
            let str = subRegex (mkRegex "Tip\\.") (getFormula $ formula) ""
            let splitStr = dropWhile ((/=) '=') str
            let up = getUserProperty formula
            putStrLn $ (if isNothing up then name else fromJust up ) ++  " " ++ splitStr
            case indVar l of
                Nothing -> return ()
                Just i  -> putStrLn $ "--- Proved using variable: " ++ (show i) 
            case hLemmas l of
                [] -> return ()
                ax -> mapM_ (\a -> putStrLn $ " | " ++ a) $ nub $ filter ((/=) name) ax

isInductive :: Lemma -> Bool
isInductive = isJust . indVar
