module Main where

import System.Process
import Data.Maybe
import Data.Either
import GHC.IO.Handle
import Tip.Parser

main :: IO()
main = do
    (_,Just handle,_,_) <- createProcess(
       proc "tip-ghc" ["Int.hs"] ){ cwd = Just "../../tools/examples"  } 
    tip_string <- hGetContents handle 
    case parse tip_string of 
        Left x          -> print $ "Failed to create theory: " ++ x
        Right x         -> print x 
    return ()

