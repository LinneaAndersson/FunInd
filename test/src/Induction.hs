{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleInstances #-}

module Induction where

import Control.Monad.State
import Prover hiding (getAxioms)


-- Monad-transformer for induction
newtype InductionT s m a = Ind (StateT s m a)
    deriving (Functor, Applicative, Monad, MonadIO, MonadState s)

-- Prover/IO instance
type Induction = InductionT (Prover,[String]) IO

getProver :: Induction Prover
getProver = fst <$> get

getAxioms :: Induction [String]
getAxioms = snd <$> get

