
module Main where

import           Control.Monad.State
import           Data.Either
import           Data.List
import           Data.List.Split
import           Data.Maybe
--import GHC.IO.Handle
import           Parser.Params
import           System.Environment
import           System.FilePath.Posix
import           System.IO
import           System.Directory
import           System.Process
import           Text.Regex
import           Tip.Core              (forallView, theoryGoals, free)
import           Tip.Formula
import           Tip.Fresh
import           Tip.Funny.Utils       (findApps)
import Tip.Funny.Property (propBody,propFunc,propGlobals)
import           Tip.Haskell.Rename
--import           Tip.Haskell.Translate
import           Tip.Lint
import           Tip.Mod
import           Tip.Parser
import           Tip.Passes
import           Tip.Pretty
import           Tip.Pretty.TFF
import           Tip.Scope
import Tip.Rename
import           Tip.Types
import           Tip.Passes.Funny

import           Constants
import           Induction.Induction
import           Induction.Types
import           Process
import           Prover                hiding (getAxioms)
import           Utils

main :: IO()
main = do

    -- parse input parameters: inout file and verbosity flags
    params <- parseParams

    -- check file extension
    preQS <- checkInputFile (inputFile params)

    -- created conjectures
    -- TODO verbosity
    prop_string <- run_process "tip-spec" "." [preQS]
    writeFile prop_file prop_string

    -- parsing tip qith quickspec to theory
    theory_qs <- readTheory prop_file

    -- Structural Induction TODO:: ADD (freshPass (monomorphise False) theory_qs)
    case induct (renameLemmas theory_qs) >>= printResult . fst of
        TP (Induct a) -> runStateT a (initState params $ getIndType params)
    return ()
        where -- count the number of conjectures in the theory
            numConj th = length $ fst $ theoryGoals th
            -- induction loop
            induct theory = loop_conj theory 0 (numConj theory) False -- :: (Name a, PrettyVar a) => Theory a -> TP a (Theory Id, Bool)
            -- initial state
            initState par ind = IndState par eprover ind [] Nothing []

-- When the input file is in haskell we need to run tip-ghc
-- to convert it to smt2
checkInputFile :: InputFile -> IO FilePath
checkInputFile (HS f)      = do
                -- start a external Process for tip-ghc, translating a haskell file
                -- into smt2 (tip-format).
                let (path, file) = splitFileName f
                tip_string <- run_process "tip-ghc" path [file]
                writeFile tip_file tip_string
                return tip_file
checkInputFile (SMT f)      = return f
checkInputFile Unrecognized = fail "Incompatible file extension"



-- Looping through all conjectures and try to prove them
-- If provable with structural induction, they are put into the theory as proven lemmas
-- If during one loop none is being proved, do not continue
-- TODO change timeout on continue
loop_conj :: Name a => Theory a -> Int -> Int -> Bool -> TP a (Theory a, Bool)
loop_conj theory curr num continue
    | curr >= num = -- tested all conjectures
        if continue -- check whether to loop again
            then loop_conj theory 0 num False   -- continue
            else return (theory, continue)      -- done
    | otherwise  = -- test next conjecture
        let
            -- pick a conjecture
            th0      = selectConjecture curr theory
            -- the formula matching the current conjecture
            formula = head . fst $ theoryGoals th0
            -- lookup up unique name of formual
            f_name  = fromMaybe "no name"  $ join $ lookup "name" (fm_attrs formula)
            -- turn formula into printable form
            formulaPrint = showFormula formula
        in do
            liftIO $ removeContentInFolder (out_path "")
            nbrVar  <- inductionSize <$> getInduction
            let th = th0
            -- clean temporary state
            modify (\s -> s{axioms = [], ind=Nothing})
            mcase (prove th) -- Test if solvable without induction
                (do -- Proved without induction
                    printStr 3 $ "| " ++ f_name  ++  "  -- P | " ++ formulaPrint
                    addLemma f_name -- add formula to proved lemmas
                    -- go to next conjecture
                    loop_conj (provedConjecture curr theory) curr (num-1) continue)
                (mcase (loop_ind th 0 (nbrVar th)) -- Attempt induction
                        (do -- Proved using induction
                            printStr 3 $ "| " ++ f_name  ++  "  -- I | " ++ formulaPrint
                            --fail $ "printout"
                            addLemma f_name -- add formula to proved lemmas
                            -- -- go to next conjecture
                            loop_conj (provedConjecture curr theory) curr (num-1) True)
                        -- Unable to prove with current theory
                        -- try next conjecture
                        (do

                            printStr 3 $ "| " ++ f_name  ++  "  -- U | " ++ formulaPrint
                            loop_conj theory (curr + 1) num continue))



-- Trying to prove a conjecture, looping over all variables in the conjecture
loop_ind :: Name a => Theory a -> Int -> Int -> TP a Bool
loop_ind theory num tot
    | num >= tot = return False -- tested all variables, unable to prove
    | otherwise  = do

        --prepare theory for induction on variable/application 'num'
        indPass <- inductionPass <$> getInduction
        let ind_theory = freshPass (indPass [num]) theory

        prep <- (prepare <$> getProver)
        liftIO $ printTheories prep ind_theory 0 (out_path ("Theory" ++ (show num)))
        --(return . show . ppTheory' . head . tff [SkolemiseConjecture])

        liftIO $ do
                      --putStrLn $ show $ map ppTheory' ind_theory
                      putStrLn "-----------------------------------------"

        mcase (proveAll ind_theory)     -- try induction on one variable
            (do -- proves using induction on 'num'
                modify (\s -> s{ind = Just num}) -- add variable used
                return True)
            (loop_ind theory (num+1) tot)   -- unable to prove, try next variable


printTheories :: Name a => (Theory a -> IO String) -> [Theory a] -> Int -> String -> IO ()
printTheories _ [] _ _              = return ()
printTheories prep (t:ts) i s       =
    do
        createDirectoryIfMissing False s
        prob <- prep t {-show . ppTheory' . tff [SkolemiseConjecture] . last $ ind_theory-}
        writeFile (s ++ "/problem" ++ (show i)) prob
        printTheories prep ts (i+1) s


removeContentInFolder :: String -> IO ()
removeContentInFolder s = do
    removeDirectoryRecursive s
    createDirectory s


-- Returns true if all conjectures are provable
proveAll :: Name a => [Theory a] -> TP a Bool
proveAll []       = return True
proveAll (th:ths) = mcase (prove th)
                        (proveAll ths)
                        (return False)


-- trying to prove one conjecture, returns true if provable
-- TODO passes should be prover dependent!!!
-- TODO Do something proper about multi-theories
prove :: Name a => Theory a -> TP a Bool
prove th = do
    {-let goal' = head $ passes th -- prepare conjecture using various passes
        goal''  = ppTheory' goal' -- turn conjecture into printable format
    in do -}
        --liftIO $ writeFile (out_path "goal.smt2") $ show $ goal''

        -- retrieve the preparation function from the Prover
        -- and apply it to the goal
        prob <- liftIO =<< (prepare <$> getProver) <*> pure th
        liftIO $ writeFile (out_path "problem") prob

        -- run prover on the problem
        ep <- runProver $ out_path "problem"

        -- check the output from the Prover by using
        -- the Provers parse function
        (b, ax) <- liftIO =<< (parseOut <$> getProver) <*> pure [prob, ep]
        -- add auxilliary lemmas to temporary state
        when b $ modify (\s -> s{axioms = axioms s ++ ax})
        return b
