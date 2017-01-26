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

example_path :: FilePath
example_path = "../../../../../../examples"

main :: IO()
main = do

    -- start a external Process for tip-ghc, translating a haskell file
    -- into smt2 (tip-format).
    (_,proc1,_,p_id) <-  createProcess( proc "tip-ghc" ["Int.hs"] )
                { cwd = Just example_path, std_out = CreatePipe  }
    waitForProcess p_id

    -- check whether tip-ghc finished correctly
    case proc1 of
        Just handle -> do
            -- Sucessfully translated file
            print "tip created!"
            tip_string <- hGetContents handle
            writeFile tip_file tip_string
            -- parsing TIP into a theory
            theory_either <- parseFile tip_file

            -- check whether the parsing succeded
            case theory_either of
                Left x  -> print $ "Failed to create theory: " ++ x
                Right theory -> do
                    -- created a theory
                    print $ "theory created!"
                    writeFile theory_file $ show theory

                    -- start a external Process for tip-spec, generating conjectures
                    -- for the parsed theory
                    (_,proc2,_,p_id) <- createProcess( proc "tip-spec" [tip_file] )
                        { cwd = Just ".", std_out = CreatePipe  }
                    waitForProcess p_id

                    -- check whether tip-spec succeded
                    case proc2 of
                        Just handle -> do
                            -- created conjectures
                            print "properties created!"
                            prop_string <- hGetContents handle
                            writeFile prop_file prop_string
                            print $ "done writing to: "
                                    ++ tip_file ++", "
                                    ++ theory_file ++ ", "
                                    ++ prop_file
                        Nothing -> print "Failed to create properties"
        Nothing -> print "could not find directory or file"
    return ()
