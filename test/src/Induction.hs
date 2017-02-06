{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleInstances #-}

module Induction where

import Control.Monad.State
import Data.List
import Prover hiding (getAxioms)
import Text.Regex


-- Monad-transformer for induction
newtype InductionT s m a = Ind (StateT s m a)
    deriving (Functor, Applicative, Monad, MonadIO, MonadState s)

-- Prover/IO instance
type Induction = InductionT (Prover,[String]) IO

getProver :: Induction Prover
getProver = fst <$> get

getAxioms :: Induction [String]
getAxioms = snd <$> get

printAxioms :: Induction ()
printAxioms = liftIO . putStrLn . unlines . nub =<< axioms  
  where
    axioms = return . map makeHandsome =<< getAxioms


makeHandsome :: String -> String
makeHandsome s = sp !! (length sp - 2)
    where sp = splitRegex (mkRegex ":") s
