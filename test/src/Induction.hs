{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleInstances #-}

module Induction where

import Control.Monad.State
import Data.List
import Prover hiding (getAxioms)
import Text.Regex
import Tip.Types
import Tip.Parser

-- Monad-transformer for induction
newtype InductionT s m a = Ind (StateT s m a)
    deriving (Functor, Applicative, Monad, MonadIO, MonadState s)

-- Prover/IO instance
type Induction = InductionT (Prover,[String],[Lemma]) IO

type Lemma =  (String, ([String], Bool))

getProver :: Induction Prover
getProver = (\(a, _, _) -> a) <$> get

getAxioms :: Induction [String]
getAxioms = (\(_, b,_) -> b) <$> get

getHelpLemmas :: String -> Induction [String]
getHelpLemmas name = lemmas =<< getLemmas
  where 
    lemmas l = case (lookup name l) of
                Nothing -> fail "Lemma not found"
                Just (list,_) -> return list        

           
getLemmas :: Induction [Lemma]
getLemmas = (\(_, _,c) -> c) <$> get

addLemma :: String -> Bool -> Induction ()
addLemma name ind = modify (\(p, t, ls) -> (p, t,((name, (t, ind)):ls)))


printAxioms :: Induction ()
printAxioms = liftIO . putStrLn . unlines . nub =<< axioms  
  where
    axioms = return . map makeHandsome =<< getAxioms


makeHandsome :: String -> String
makeHandsome s = sp !! (length sp - 2)
    where sp = splitRegex (mkRegex ":") s
