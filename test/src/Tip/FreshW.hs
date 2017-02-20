{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses      #-}

module Tip.FreshW where

import Control.Monad
import Control.Monad.Writer
import Tip.Fresh

type FreshWriter w a = (WriterT w Fresh a)
    --deriving (Functor, Applicative, Monad, MonadWriter w)

type FreshW = FreshWriter [String]

getFresh :: FreshWriter w a -> -> IO a
getFresh fw = mapM_ putStrLn w >> return a
    where (a, w) = runWriter fw
