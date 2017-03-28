{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses      #-}

module Induction.Types where

import           Control.Monad.State (MonadIO, MonadState, MonadTrans, StateT,
                                      get)
import           Control.Exception.Base (SomeAsyncException)
import           Control.Monad.Error ( MonadError)  
import           Data.List           (find)

import           Tip.Fresh           (Fresh (..), Name)
import           Tip.Types           (Theory (..), Formula(..))

import           Parser.Params       (Params (..))
import           Prover              (Prover (..))

-- Monad-transformer for induction
--type TheoremProverT s m a = StateT s m a
    --deriving (Functor, Applicative, Monad, MonadIO, MonadState s, MonadTrans)

data Name a => Induction a = Ind
    {   inductionSize :: Theory a -> Int
    ,   inductionPass :: [Int] -> Theory a -> Fresh [Theory a]
    ,   printVar      :: Theory a -> [Int] -> Formula a -> String
    }

data Name a => IndState a = IndState
  {  params :: Params
  ,  prover :: Prover a
  ,  induct :: Induction a
  ,  lemmas :: [Lemma]
  ,  ind    :: Maybe [Int]
  ,  axioms :: [String]
  }

-- Prover/IO instance
newtype Name a => TP a b = TP (StateT (IndState a) IO b)
    deriving (Functor, Applicative, Monad, MonadIO, MonadState (IndState a))

data Lemma = Lemma
    { lemmaName   :: String
    , lemmaSource :: Maybe String
    , hLemmas     :: [String]
    , indVar      :: Maybe [Int]
    , formula     :: String
    } deriving (Show,Read)

getInduction :: Name a => TP a (Induction a)
getInduction = induct <$> get

getProver :: Name a => TP a (Prover a)
getProver = prover <$> get

getAxioms :: Name a => TP a [String]
getAxioms = axioms <$> get

getHelpLemmas :: Name a => String -> TP a [String]
getHelpLemmas name = lemmas =<< getLemmas
  where
    lemmas ls = case find ((==) name . lemmaName) ls of
                    Nothing -> fail "Lemma not found"
                    Just l  -> return $ hLemmas l

getVar :: Name a => TP a (Maybe [Int])
getVar = ind <$> get

getLemmas :: Name a => TP a [Lemma]
getLemmas = lemmas <$> get
