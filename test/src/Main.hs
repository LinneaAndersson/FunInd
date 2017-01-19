module Main where

import System.Process
import Data.Maybe
import GHC.IO.Handle

main :: IO()
main = withFile "$D/Master_Thesis/repositories/FunInd/test/out.smt2" WriteMode $ \out -> do
        r <- createProcess(
                --proc "tip-ghc" ["/home/linnea/Master_thesis/tools/examples/Int.hs"] )
                proc "tip-ghc" ["$D/Master_Thesis/repositories/tools/examples/Int.hs"] ) 
                { std_out = UseHandle out } 
--        kkk <- hGetContents h 
--        print kkk
        return ()
