module Main where

import System.Process
import Data.Maybe
import GHC.IO.Handle

main :: IO()
main = do 
        (_,Just h,_,_) <- createProcess(
                proc "tip-ghc" ["/home/linnea/Master_thesis/tools/examples/Int.hs"] ) { std_out = CreatePipe } 
        kkk <- hGetContents h 
        print kkk
        return ()
