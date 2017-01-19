module Main where

import System.Process
import Data.Maybe
import GHC.IO.Handle

main :: IO()
main = do
        (_,Just h,_,_) <- createProcess(
           proc "tip-ghc" ["Int.hs"] ){ cwd = Just "../../tools/examples"  } 
        kkk <- hGetContents h 
        print kkk
        return ()


--withFile "$D/Master_Thesis/repositories/FunInd/test/out.smt2" WriteMode $ \out -> do
 --proc "tip-ghc" ["$D/Master_Thesis/repositories/tools/examples/Int.hs"] ) 
                --{ std_out = UseHandle out } 
