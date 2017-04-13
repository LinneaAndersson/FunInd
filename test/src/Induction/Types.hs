{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses      #-}

module Induction.Types where

import           Control.Monad.State (MonadIO, MonadState, MonadTrans, StateT,
                                      get)
import           Control.Exception.Base (SomeAsyncException)
import           Control.Monad.Error ( MonadError)  
import           Data.List           (find, nub, (\\))

import           Tip.Fresh           (Fresh (..), Name)
import           Tip.Types           (Theory (..), Formula(..))

import           Parser.Params       (Params (..))
import           Prover              (Prover (..))

-- Monad-transformer for induction
--type TheoremProverT s m a = StateT s m a
    --deriving (Functor, Applicative, Monad, MonadIO, MonadState s, MonadTrans)


type Condition = (Maybe [Int], [String])

elemC :: Condition -> [Condition] -> Bool
elemC c [] = False
elemC a (b:bs) = (null $ (a' \\ b') ++ (b' \\ a')) || (elemC a bs) 
        where 
            a' = nub $ snd a
            b' = nub $ snd b

type IndPass a = [Int] -> Theory a -> Fresh [Theory a]

data Name a => Induction a = Ind
    {   inductionSize :: Theory a -> Int
    ,   inductionPass :: [IndPass a]
    ,   printVar      :: Theory a -> [Int] -> Formula a -> String
    ,   selectConj    :: Theory a -> TP a (Theory a)
    ,   provedConj    :: Theory a -> TP a (Theory a) 
    }

data Name a => IndState a = IndState
  {  params :: Params
  ,  prover :: Prover a
  ,  induct :: Induction a
  ,  lemmas :: [Lemma]
  ,  axioms :: [String]
  ,  loopNbr:: Int
  }

-- Prover/IO instance
newtype Name a => TP a b = TP (StateT (IndState a) IO b)
    deriving (Functor, Applicative, Monad, MonadIO, MonadState (IndState a))

data Lemma = Lemma
    { lemmaName   :: String
    , lemmaSource :: Maybe String
    , hLemmas     :: [(Maybe [Int], [String])]
    , formula     :: String
    , status      :: Bool 
    } deriving (Show,Read)

getInduction :: Name a => TP a (Induction a)
getInduction = induct <$> get

getProver :: Name a => TP a (Prover a)
getProver = prover <$> get

getAxioms :: Name a => TP a [String]
getAxioms = axioms <$> get

getLoopNbr :: Name a => TP a Int
getLoopNbr = loopNbr <$> get

getLemmas :: Name a => TP a [Lemma]
getLemmas = lemmas <$> get
