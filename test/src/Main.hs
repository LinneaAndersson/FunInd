
module Main where

import           Control.Monad.State   (get, join, liftIO, modify, runStateT,
                                        when)
import           Data.Maybe            (fromMaybe)
import           System.FilePath.Posix (splitFileName)
import           System.Directory      (createDirectory,
                                        createDirectoryIfMissing,
                                        removeDirectoryRecursive)
import           System.CPUTime         (getCPUTime)
import           Data.Time              (getCurrentTime,diffUTCTime)

import           Tip.Core              (theoryGoals)
import           Tip.Formula           (showFormula)
import           Tip.Fresh             (Name, freshPass)
import           Tip.Mod               (renameLemmas)
import           Tip.Passes            (StandardPass(..),provedConjecture, selectConjecture, runPasses, freshPass)
import           Tip.Types             (Theory, fm_attrs)
import           Tip.Pretty.SMT        (ppTheory)

import           Constants             (out_path, prop_file, tip_file)
import           Induction.Induction   (addLemma, getIndType, nextTimeout,
                                        printResult, printStr, runProver)
import           Induction.Types       (IndState (..), Induction (..), TP (..),
                                        TheoremProverT (..), getInduction,
                                        getProver)
import           Parser.Params         (InputFile (..), Params (..),
                                        TheoremProver (..), parseParams)
import           Process               (readTheory, run_process)
import           Prover                (Prover (..), eprover)
import           Utils                 (mcase)

main :: IO()
main = do
    
    start <- getCurrentTime  
    
    -- parse input parameters: inout file and verbosity flags
    params <- parseParams

    when (outputLevel params >= 4) $ putStrLn $ show params

    -- check file extension
    preQS <- checkInputFile (inputFile params)

        -- created conjectures
    -- TODO verbosity
    prop_string <- if (tipspec params) 
                        then run_process "tip-spec" "." [preQS]
                        else readFile preQS  
    writeFile prop_file prop_string

    --theory <- readTheory preQS
    
    -- parsing tip qith quickspec to theory
    theory_qs <- readTheory prop_file


    let theory' = theory_qs--head . freshPass (runPasses [TypeSkolemConjecture, Monomorphise False]) $ theory_qs

    --writeFile (out_path "monoSkolem") . show . ppTheory [] $ theory'
    -- Structural Induction TODO:: ADD (freshPass (monomorphise False) theory_qs)
    case induct (renameLemmas theory') >>= printResult . fst of
        TP (Induct a) -> runStateT a (initState params)

    end <- getCurrentTime
    let diff = diffUTCTime end start--(fromIntegral (end - start)) / (10^12)
    when  (outputLevel params>0) $ do   
        putStrLn ""
        putStrLn $  "Time taken: " ++ (show diff) ++ "sec"
    return ()
        where -- count the number of conjectures in the theory
            numConj th = length $ fst $ theoryGoals th
            -- induction loop
            induct theory = do
                pstr <- show <$> getProver
                printStr 4 pstr
                loop_conj theory 0 (numConj theory) False

initState :: Name a => Params -> IndState a
initState par = IndState par
    (selectProver par)
    (getIndType par)
    [] Nothing []

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

selectProver ::Name a => Params -> Prover a
selectProver p = case backend p of
                    E -> eprover

-- Looping through all conjectures and try to prove them
-- If provable with structural induction, they are put into the theory as proven lemmas
-- If during one loop none is being proved, do not continue
loop_conj :: Name a => Theory a -> Int -> Int -> Bool -> TP a (Theory a, Bool)
loop_conj theory curr num continue
    | curr >= num = -- tested all conjectures
        if continue -- check whether to loop again
            then loop_conj theory 0 num False   -- continue
            else mcase (nextTimeout)
                    (loop_conj theory 0 num False)
                    (return (theory, continue))      -- done
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
                    loop_conj (provedConjecture curr theory) curr (num-1) True)
                (mcase (loop_ind th 0 (nbrVar th)) -- Attempt induction
                        (do -- Proved using induction
                            --fail "hello"
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

        prep <- prepare <$> getProver
        liftIO $ printTheories prep ind_theory 0 (out_path ("Theory" ++ show num))
        --(return . show . ppTheory' . head . tff [SkolemiseConjecture])

        printStr 3 "-----------------------------------------"
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
        writeFile (s ++ "/problem" ++ show i) prob
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
