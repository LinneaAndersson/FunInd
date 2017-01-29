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
    tip_string <- run_process "tip-ghc" path [file]
    print "tip created!"
    writeFile tip_file tip_string

    -- parsing TIP into a theory
    theory <- readTheory tip_file
    print $ "theory created!"
    writeFile theory_file $ show $ theory

    -- created conjectures
    prop_string <- run_process "tip-spec" "." [tip_file]
    print "properties created!"
    writeFile prop_file prop_string
    print $ "done writing to: "
            ++ tip_file ++", "
            ++ theory_file ++ ", "
            ++ prop_file
    test
    return ()

readTheory :: FilePath -> IO (Theory Id)
readTheory fp = do
  theory_either <- parseFile fp
  -- check whether the parsing succeded
  case theory_either of
      Left x  -> fail $ "Failed to create theory: " ++ x
      Right theory -> return theory

passes :: Theory Id -> [Theory Id]
passes = freshPass (runPasses
        [ SkolemiseConjecture, TypeSkolemConjecture
          , Monomorphise False
          , LambdaLift, AxiomatizeLambdas
          , SimplifyGently, CollapseEqual, RemoveAliases
          , SimplifyGently, Monomorphise False, IfToBoolOp, CommuteMatch
          , SimplifyGently, LetLift, SimplifyGently, AxiomatizeFuncdefs2
          , SimplifyGently, AxiomatizeDatadecls
        ])
{-
      [  TypeSkolemConjecture, Monomorphise False
          , SimplifyGently, LambdaLift, AxiomatizeLambdas, Monomorphise False
          , SimplifyGently, CollapseEqual, RemoveAliases
          , SimplifyGently
          , AxiomatizeFuncdefs2
          , RemoveMatch -- in case they appear in conjectures
          , SkolemiseConjecture
          , NegateConjecture
      ])
-}
test :: IO()
test = do
  theory <- readTheory prop_file
  let goal = selectConjecture 0 theory
  --let goal = axiomatizeFuncdefs theory'
  let goal' = head $ passes ({-head $ freshPass (induction [0])-} goal)
  let goal'' = ppTheory goal'
  writeFile (out_path "goal1.smt2") $ show $ goal
  writeFile (out_path "goal2.smt2") $ show $ goal'
  writeFile (out_path "goal.smt2") $ show $ goal''
  jukebox (out_path "goal.smt2")
  return ()


jukebox :: FilePath -> IO String
jukebox source = fmap ((++) "jukbox done: ") $ run_process jb "." ["fof", source]
        where jb = ".stack-work/install/x86_64-linux/lts-7.7/8.0.1/bin/jukebox"

-- name -> cwd -> optional args -> output
run_process :: String -> FilePath -> [String] -> IO String
run_process name path ops = do
    (_,proc,_,p_id) <- createProcess( proc name ops )
        { cwd = Just path, std_out = CreatePipe  }
    waitForProcess p_id
    case proc of
        Nothing     -> fail $ "Error: Could not run '" ++ name ++ "'"
        Just handle -> hGetContents handle
