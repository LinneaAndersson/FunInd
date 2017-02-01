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
import Tip.Lint

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
    print "theory created!"
    writeFile theory_file $ show theory

    -- created conjectures
    prop_string <- run_process "tip-spec" "." [tip_file]
    print "properties created!"
    writeFile prop_file prop_string

    -- parsing tip qith quickspec to theory
    theory_qs <- readTheory prop_file

    -- Structural Induction
    loop_conj theory_qs 0 (numConj theory_qs) False

    return ()
        where -- count the number of conjectures in the theory
            numConj th = length $ fst $ theoryGoals th

-- read a theory from a file with given filepath
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
        [ SkolemiseConjecture
          , TypeSkolemConjecture
          , Monomorphise False
          , LambdaLift
          , AxiomatizeLambdas
          , SimplifyGently
          , CollapseEqual
          , RemoveAliases
          , SimplifyGently
          , Monomorphise False
          , IfToBoolOp
          , CommuteMatch
          , SimplifyGently
          , LetLift
          , SimplifyGently
          , AxiomatizeFuncdefs2
          , SimplifyGently
          , AxiomatizeDatadecls
        ])

-- Looping through all conjectures and try to prove them
-- If provable with structural induction, they are put into the theory as proven lemmas
-- If during one loop none is being proved, do not continue
loop_conj :: Theory Id -> Int -> Int -> Bool -> IO (Theory Id, Bool)
loop_conj theory curr num continue
    | curr >= num = -- tested all conjectures
        if continue -- check whether to loop again
            then loop_conj theory 0 num False   -- continue
            else return (theory, continue)      -- done
    | otherwise  = -- test next conjecture
        let
            -- pick a conjecture
            th = selectConjecture curr theory
            -- find all variables
            vars =  fst . forallView $ fm_body $ head . fst $ theoryGoals th
            -- count variables, to be used in induction loop
            nbrVar = length vars

            formula = showFormula (fst ( theoryGoals theory ) !! curr)
        in do
            print "------- Now considering: -------"
            print $ "| " ++ formula
            mcase (prove E th) -- Test if solvable without induction
                (do -- Proved without induction
                    print $ ":)  -->   " ++ formula
                    loop_conj (deleteConjecture curr theory) curr (num-1) continue)
                (do
                    print "| Try with induction"
                    writeFile (out_path "goal2.smt2") $ show $ ppTheory th
                    mcase (loop_ind th nbrVar) -- Attempt induction
                        (do -- Proved using induction
                            print $ ":D  -->   " ++ formula
                            loop_conj (provedConjecture curr theory) curr (num-1) True)
                        (do -- Unable to prove with current theory
                            print $ ":(  -->    " ++ formula
                            loop_conj theory (curr + 1) num continue))

-- Stringify the body of a Formula
-- TODO remove \"
showFormula :: (Ord a, PrettyVar a) => Formula a -> String
showFormula fa =  concatMap repl splitStr
    where
        str = show $ ppExpr 0 $ fm_body fa
        splitStr = last (splitOn "\n" str)
        repl '\\' = []
        repl '"' = []
        repl c = [c]


-- Trying to prove a conjecture, looping over all variables in the conjecture
loop_ind :: Theory Id -> Int -> IO Bool
loop_ind theory 0   = return False  -- tested all variables, unable to prove
loop_ind theory num = do
    print $ "Ind on: " ++ show (num - 1)
    mcase (proveAll ind_theory)     -- try induction on one variable
        (return True)               -- proves using induction on 'num'
        (loop_ind theory (num-1))   -- unable to prove, try next variable
    where -- prepare theory for induction on variable 'num'
        ind_theory = freshPass (induction [num-1]) theory

-- Returns true if all conjectures are provable
proveAll :: [Theory Id] -> IO Bool
proveAll []       = return True
proveAll (th:ths) = mcase (prove E th)
                        (proveAll ths)
                        (return False)

-- case of for monadic bool
mcase :: (Monad m) => m Bool -> m a -> m a -> m a
mcase mbool t f = do
    bool <- mbool
    if bool then t else f

-- trying to prove one conjecture, returns true if provable
-- TODO make 'Prover' dependent
prove :: Prover -> Theory Id -> IO Bool
prove p th =
    let goal' = head $ passes th -- prepare conjecture using various passes
        goal'' = ppTheory goal'
    in do
        writeFile (out_path "goal.smt2") $ show $ goal''
        writeFile (out_path "goal1.smt2") $ show $ ppTheory th

        prob <- jukebox_hs $ show goal''
        writeFile (out_path "jb.fof") $ showProblem prob

        -- run prover on the problem
        ep <- eprover $ out_path "jb.fof"

        -- check result
        case Ep.extractAnswer prob ep of
            -- a problem occured
            Right terms -> do
                            print "Could not extract the answer from the prover"
                            return False
            {-
                Check the answer. The E prover attempts to prove negations, thus
                a problem should NOT be 'Satisfiable'.
            -}
            Left ans -> case ans of
                Satisfiable     -> return False
                Unsatisfiable   -> return True
                NoAnswer reason ->
                    -- print $ "Could not find the answer:  " ++ (show reason)
                    return False

-- Calls Jukebox to turn a tff problem into fof
jukebox_hs :: String -> IO (Problem Form)
jukebox_hs problem =
    let pars =  toFofBox    -- Box to turn tff to fof
        p = parser pars     -- grab the parser
    in do
        -- parse the problem
        problemForm <- parseString problem
        -- run jukebox quietly
        case runPar p ["--quiet"] of
            Right mToFof -> do
                toFof <- mToFof     -- extractin fof-function from monad
                toFof problemForm   -- translate tff to fof

            -- Somethin unexpected occured
            -- TODO do omething with error
            Left err -> fail "Error: jukebox kunde inte köras"


-- run eprover on the file given by the filepath
-- TODO generalise for options
eprover :: FilePath -> IO String
eprover source = run_process "eprover" "." ["--tstp-in", "--auto", "--silent", "--soft-cpu-limit=5", source]

-- run a process with name, working dir path, and a list of arguments
-- return any output generated by the command
run_process :: String -> FilePath  -> [String] -> IO String
run_process name path  ops = do
    (_,proc,_,p_id) <- createProcess( proc name ops )
        { cwd = Just path, std_out = CreatePipe  }
    waitForProcess p_id
    case proc of
        Nothing     -> fail $ "Error: Could not run '" ++ name ++ "'"
        Just handle -> hGetContents handle
