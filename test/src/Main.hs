module Main where

import System.Process
import Data.Maybe
import Data.Either
import GHC.IO.Handle
import Tip.Parser
import System.IO

tip_file :: FilePath
tip_file = "tip_file.smt2"

theory_file :: FilePath
theory_file = "theory_file.txt"

prop_file :: FilePath
prop_file = "prop.txt"

main :: IO()
main = do
    (_,proc1,_,p_id) <-  createProcess( proc "tip-ghc" ["Int.hs"] )
                { cwd = Just "../../../../../../examples", std_out = CreatePipe  }
    waitForProcess p_id
    case proc1 of
        Just handle -> do  
            print "tip created!"
            tip_string <- hGetContents handle 
            writeFile tip_file tip_string
            theory_either <- parseFile tip_file
            case theory_either of 
                Left x  -> print $ "Failed to create theory: " ++ x           
                Right theory -> do 
                    print $ "theory created!"
                    writeFile theory_file $ show theory 
                    (_,proc2,_,p_id) <- createProcess( proc "tip-spec" [tip_file] ) 
                        { cwd = Just ".", std_out = CreatePipe  }
                    waitForProcess p_id
                    case proc2 of
                        Just handle -> do 
                            print "properties created!"
                            prop_string <- hGetContents handle
                            writeFile prop_file prop_string
                            print $ "done writing to: " ++ tip_file ++", " ++ theory_file ++ ", " ++ prop_file
                        Nothing -> print "Failed to create properties"
        Nothing -> print "could not find directory or file"
    return ()

