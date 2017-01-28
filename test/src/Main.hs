module Main where

import System.Process
import Data.Maybe
import Data.Either
import GHC.IO.Handle
import Tip.Parser
import Tip.Types
import Tip.Core (theoryGoals)
import Tip.Passes
import System.IO
import System.Environment
import System.FilePath.Posix
import Tip.Pretty.TFF

tip_file :: FilePath
tip_file = out_path "tip_file.smt2"

theory_file :: FilePath
theory_file = out_path "theory_file.txt"

prop_file :: FilePath
prop_file = out_path "prop.txt"


out_path :: FilePath -> FilePath
out_path = (++) "./out/"

main :: IO()
main = do
    
    -- query for input file
    (a:args) <- getArgs
    let (path, file) = splitFileName a
 
    -- start a external Process for tip-ghc, translating a haskell file
    -- into smt2 (tip-format).
    (_,proc1,_,p_id) <-  createProcess( proc "tip-ghc" [file] )
                { cwd = Just path, std_out = CreatePipe  }
    waitForProcess p_id

    -- check whether tip-ghc finished correctly
    case proc1 of
        Just handle -> do
            -- Sucessfully translated file
            print "tip created!"
            tip_string <- hGetContents handle
            writeFile tip_file tip_string

            -- parsing TIP into a theory
            theory <- readTheory tip_file
            print $ "theory created!"
            writeFile theory_file $ show $ theory

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
                    test
                Nothing -> print "Failed to create properties"
        Nothing -> print "could not find directory or file"
    return ()

readTheory :: FilePath -> IO (Theory Id)
readTheory fp = do
  theory_either <- parseFile fp
  -- check whether the parsing succeded
  case theory_either of
      Left x  -> fail $ "Failed to create theory: " ++ x
      Right theory -> return theory

passes :: Theory Id -> [Theory Id]
passes = freshPass (runPasses [SkolemiseConjecture])

test :: IO()
test = do
  theory <- readTheory prop_file
  let goal = selectConjecture 0 theory
  --let goal = axiomatizeFuncdefs theory'
  let goal' = head $ freshPass (runPasses      
        [  TypeSkolemConjecture, Monomorphise False
            , SimplifyGently, LambdaLift, AxiomatizeLambdas, Monomorphise False
            , SimplifyGently, CollapseEqual, RemoveAliases
            , SimplifyGently
            , AxiomatizeFuncdefs2
            , RemoveMatch -- in case they appear in conjectures
            , SkolemiseConjecture
            , NegateConjecture
        ]) goal
  let goal'' = ppTheory goal'
  writeFile (out_path "goal1.smt2") $ show $ goal
  writeFile (out_path "goal2.smt2") $ show $ goal'
  writeFile (out_path "goal.smt2") $ show $ goal''
  jukebox (out_path "goal1.smt2")
  return ()


jukebox :: FilePath -> IO()
jukebox source = do
  (_,proc1,_,p_id) <-  createProcess( proc jb ["fof",source] )
              { cwd = Just ".", std_out = CreatePipe  }
  waitForProcess p_id
  case proc1 of
    Nothing -> fail $ "could not run jukebox: "
    Just handle -> do
        content <- hGetContents handle
        print $ "jukbox done: " ++ content
  return ()
    where 
        jb = ".stack-work/install/x86_64-linux/lts-7.7/8.0.1/bin/jukebox"
    
