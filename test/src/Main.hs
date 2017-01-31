module Main where

import Data.List.Split
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
import Tip.Pretty

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

    -- parsing tip qith quickspec to theory
    theory_qs <- readTheory prop_file

    -- Induction
    loop_conj theory_qs 0 (numConj theory_qs) False
    
    return ()
        where numConj th = length $ fst $ theoryGoals th

readTheory :: FilePath -> IO (Theory Id)
readTheory fp = do
  theory_either <- parseFile fp
  -- check whether the parsing succeded
  case theory_either of
      Left x  -> fail $ "Failed to create theory: " ++ x
      Right theory -> return theory

-- The passes needed to convert the theory into tff format (+ skolemiseconjecture) 
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

-- Looping through all conjectures and try to prove them
-- If provable with structural induction, they are put into the theory as proven lemmas
-- If during one loop none is being proved, do not continue
loop_conj :: Theory Id -> Int -> Int -> Bool -> IO (Theory Id, Bool)
loop_conj theory curr num continue
    | curr >= num = do  
        case continue of
            False -> return (theory, continue)
            True -> loop_conj theory 0 num False
    | otherwise  = do
        
        let th = selectConjecture curr theory
        let vars =  fst . forallView $ fm_body $ head . fst $ theoryGoals th
        let nbrVar = length vars
        mcase (prove E th)
            (do
                print $ ":)  -->   " ++ ( show $ printConj ( fm_body ((fst ( theoryGoals theory ) )!!curr) ))
                loop_conj (deleteConjecture curr theory) curr (num-1) continue)
            (mcase (loop_ind th nbrVar)
                (do
                    print $ ":D  -->   " ++ ( show $ printConj ( fm_body ((fst ( theoryGoals theory ) )!!curr) ))
                    loop_conj (provedConjecture curr theory) curr (num-1) True) --proved
                (do
                    print $ ":(  -->    " ++ ( show $ printConj ( fm_body ((fst ( theoryGoals theory ) )!!curr) ))
                    loop_conj theory (curr + 1) num continue)) -- not proves


printConj :: (Ord a, PrettyVar a) => Expr a -> String
printConj e =  concatMap repl splitStr
    where 
        exp = ppExpr 0 e
        str = show exp
        splitStr = last (splitOn "\n" str)
        repl '\\' = []
        repl '"' = [] 
        repl c = [c]       


-- Trying to prove a conjecture, looping over all variables in the conjecture
loop_ind :: Theory Id -> Int -> IO Bool
loop_ind theory 0   = return False
loop_ind theory num = mcase (proveAll ind_theory)
                        (return True)
                        (loop_ind theory (num-1))
                        where ind_theory = freshPass (induction [num-1]) theory

-- Returns true if all conjectures are provable
proveAll :: [Theory Id] -> IO Bool
proveAll []       = return True
proveAll (th:ths) = mcase (prove E th)
                        (proveAll ths)
                        (return False)

-- case of monadic bool
mcase :: (Monad m) => m Bool -> m a -> m a -> m a
mcase mbool t f = do
    bool <- mbool
    case bool of
        True  -> t
        False -> f

-- trying to prove one conjecture, returns true if provable
prove :: Prover -> Theory Id -> IO Bool
prove p th = do
    let goal' = head $ passes th
    let goal'' = ppTheory goal'

    --writeFile (out_path "goal.smt2") $ show goal''

    prob <- jukebox_hs $ show goal'' 
    writeFile (out_path "jb.fof") $ showProblem prob
    ep <- eprover $ out_path "jb.fof"
    case Ep.extractAnswer prob ep of
        Right err -> do print "Could not extract the answer from the prover"
                        return False
        Left ans -> case ans of
            Satisfiable -> return False
            Unsatisfiable -> return True
            NoAnswer reason -> do
                -- print $ "Could not find the answer:  " ++ (show reason)
                return False

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
eprover source = run_process "eprover" "." ["--tstp-in", "--auto", "--silent", "--soft-cpu-limit=5", source]

run_process :: String -> FilePath  -> [String] -> IO String
run_process name path  ops = do
    (_,proc,_,p_id) <- createProcess( proc name ops )
        { cwd = Just path, std_out = CreatePipe  }
    waitForProcess p_id
    case proc of
        Nothing     -> fail $ "Error: Could not run '" ++ name ++ "'"
        Just handle -> hGetContents handle
