module Main where

import System.Process
import Data.Maybe
import Data.Either
import GHC.IO.Handle
import Tip.Parser
import System.IO

tip_file :: FilePath
tip_file = "tip_file.smt2"

main :: IO()
main = do
    (_,proc,_,p_id) <-  createProcess( proc "tip-ghc" ["Int.hs"] )
                { cwd = Just "../../../../../../../../tools/examples", std_out = CreatePipe  }
    waitForProcess p_id
    case proc of
		Just handle -> do  
			tip_string <- hGetContents handle 
			writeFile tip_file tip_string
			theory_either <- parseFile tip_file
			case theory_either of 
				Left x  -> print $ "Failed to create theory: " ++ x           
				Right theory -> print "theory created" 
		Nothing -> print "could not find directory or file"
    return ()

