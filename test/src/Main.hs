module Main where

import Jukebox.Form
import Jukebox.Options
import Jukebox.Toolbox
import Jukebox.TPTP.Parse
import Jukebox.TPTP.Print
import qualified Jukebox.Provers.E as Ep
import Data.List
import System.Process
import Data.Maybe
import Data.Either
--import GHC.IO.Handle
import Tip.Parser
import Tip.Types
import Tip.Core (theoryGoals, forallView)
import Tip.Passes
import System.IO
import System.Environment
import System.FilePath.Posix
import Tip.Pretty.TFF
import Tip.Scope


data Prover = E

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
    -- query for input
    (path, file) <- (splitFileName . head) <$> getArgs

    -- start a external Process for tip-ghc, translating a haskell file
    -- into smt2 (tip-format).
--    run_process "tip-ghc" path tip_file [file]
    print "tip created!"
    --writeFile tip_file tip_string

    -- parsing TIP into a theory
    --theory <- readTheory tip_file
    print $ "theory created!"
  --  writeFile theory_file $ show $ theory

    -- created conjectures
    --run_process "tip-spec" "." prop_file [tip_file]
    --print "properties created!"
    --writeFile prop_file prop_string
    print $ "done writing to: "
            ++ tip_file ++", "
            ++ theory_file ++ ", "
            ++ prop_file

    -- parsing tip qith quickspec to theory
    theory_qs <- readTheory prop_file

--    print $ fst $ theoryGoals theory_qs
    -- Induction
    loop_conj theory_qs 0 (numConj theory_qs) False
    -- theory_to_fof
    return ()
        where numConj th = length $ fst $ theoryGoals th

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
{-
theory_to_fof :: IO ()
theory_to_fof = do
    theory <- readTheory prop_file
    let goal = selectConjecture 0 theory
    let goal' = head $ passes ({-head $ freshPass (induction [0])-} goal)
    let goal'' = ppTheory goal'


    writeFile (out_path "goal1.smt2") $ show goal
    writeFile (out_path "goal2.smt2") $ show goal'
    writeFile (out_path "goal.smt2") $ show goal''



    str <- jukebox (out_path "goal.smt2")
    writeFile (out_path "goal.fof") $ str
    str2 <- eprover (out_path "goal.fof")
    print str2
    return ()
-}
loop_conj :: Theory Id -> Int -> Int -> Bool -> IO (Theory Id, Bool)
loop_conj theory curr num continue
    | curr >= num = do  
        print $ "curr   " ++ (show curr)
        print $ "num   " ++ (show num) 
        case continue of
            False -> return (theory, continue)
            True -> loop_conj theory 0 num False
    | otherwise  = do
        print $ "current:" ++ (show curr)
        print $ "num:" ++ (show num)
         
        let vars =  fst . forallView $ fm_body $ head . fst $ theoryGoals theory
        let nbrVar = length vars
        let th = selectConjecture curr theory
        mcase (prove E th)
            (do
                print $ ":)  -->  " ++ ( show $ fm_attrs ((fst ( theoryGoals theory ) )!!curr) )
                loop_conj (deleteConjecture curr theory) curr (num-1) continue)
            (mcase (loop_ind th nbrVar)
                (do
                    print $ ":D -->  " ++ ( show $ fm_attrs ((fst ( theoryGoals theory ) )!!curr) )
                    loop_conj (provedConjecture curr theory) curr (num-1) True) --proved
                (do
                    print ":("
                    loop_conj theory (curr + 1) num continue)) -- not proves
{-                where
                    vars =  fst . forallView $ fm_body $ head . fst $ theoryGoals theory
                    nbrVar = length vars
                    th = selectConjecture curr theory
-}
loop_ind :: Theory Id -> Int -> IO Bool
loop_ind theory 0   = return False
loop_ind theory num = mcase (proveAll ind_theory)
                        (return True)
                        (loop_ind theory (num-1))
                        where ind_theory = freshPass (induction [num-1]) theory


proveAll :: [Theory Id] -> IO Bool
proveAll []       = return True
proveAll (th:ths) = mcase (prove E th)
                        (proveAll ths)
                        (return False)


    --(conj, ass) <- theoryGoals th
    --length conj


mcase :: (Monad m) => m Bool -> m a -> m a -> m a
mcase mbool t f = do
    bool <- mbool
    case bool of
        True  -> t
        False -> f

prove :: Prover -> Theory Id -> IO Bool
prove p th = do
    let goal' = head $ passes th
    let goal'' = ppTheory goal'

    --writeFile (out_path "goal.smt2") $ show goal''

    prob <- jukebox_hs $ show goal'' --(out_path "goal.smt2")
    writeFile (out_path "jb.fof") $ showProblem prob
    --ans <- Ep.runE (Ep.EFlags {Ep.eprover="eprover",Ep.timeout=Just 10,Ep.memory=Nothing}) prob
    ep <- eprover $ out_path "jb.fof"
    case Ep.extractAnswer prob ep of
        Right err -> do print "Termer ??"
                        return False
        Left ans -> case ans of
            Satisfiable -> return False
            Unsatisfiable -> return True
            NoAnswer reason -> do
                print $ "Could not find the answer:  " ++ (show reason)
                return False
    --print prob
    --return False
    --str <- jukebox (out_path "goal.smt2")
    --writeFile (out_path "goal.fof") $ str
    --str2 <- eprover (out_path "goal.fof")
    --return (isInfixOf "Proof found" str2)

{-
jukebox :: FilePath -> IO String
jukebox source = run_process jb "." ["fof", source]
        where jb = ".stack-work/install/x86_64-linux/lts-7.7/8.0.1/bin/jukebox"
-}

jukebox_hs :: String -> IO (Problem Form)
jukebox_hs problem = do
    problemForm <- parseString problem
    let pars =  toFofBox 
    let p = parser pars
    case (runPar p ["--quiet"]) of
        Right r -> do
            fun <- r
            fun problemForm
        Left err -> fail $ "Error: jukebox kunde inte köras"


eprover :: FilePath -> IO String
eprover source = run_process2 "eprover" "." ["--tstp-in", "--auto", "--silent", "--soft-cpu-limit=5", source]

-- name -> cwd -> optional args -> output
run_process :: String -> FilePath -> FilePath -> [String] -> IO ()
run_process name path out_file ops = do
    file_h <- openFile out_file WriteMode
    (_,proc,_,p_id) <- createProcess( proc name ops )
        { cwd = Just path, std_out = UseHandle file_h  }
    waitForProcess p_id
    hClose file_h
{-    case proc of
        Nothing     -> fail $ "Error: Could not run '" ++ name ++ "'"
        Just handle -> return ()
-}

run_process2 :: String -> FilePath  -> [String] -> IO String
run_process2 name path  ops = do
    (_,proc,_,p_id) <- createProcess( proc name ops )
        { cwd = Just path, std_out = CreatePipe  }
    waitForProcess p_id
    case proc of
        Nothing     -> fail $ "Error: Could not run '" ++ name ++ "'"
        Just handle -> hGetContents handle
