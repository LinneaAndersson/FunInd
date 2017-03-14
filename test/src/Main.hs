module Main where

import           Control.Monad.State   (get, join, liftIO, modify, runStateT, filterM,
                                        when)
import           Control.Exception     (Exception, SomeException, catch)
import           Data.Maybe            (fromMaybe)
import           System.FilePath.Posix (splitFileName,
                                        takeBaseName,
                                        takeDirectory,
                                        normalise,
                                        splitExtension,
                                        takeFileName)
import           System.Directory      (createDirectory,
                                        createDirectoryIfMissing,
                                        removeDirectoryRecursive,
                                        doesFileExist,
                                        getDirectoryContents)
import           System.CPUTime         (getCPUTime)
import           Data.Time              (getCurrentTime,diffUTCTime)

import           Tip.Core              (theoryGoals)
import           Tip.Formula           (showFormula)
import           Tip.Fresh             (Name, freshPass)
import           Tip.Mod               (renameLemmas)
import           Tip.Passes            (StandardPass(..),provedConjecture, selectConjecture, runPasses, freshPass)
import           Tip.Types             (Theory, fm_attrs)
import           Tip.Pretty.SMT        (ppTheory)

import           Constants             (out_path, prop_file, tip_file, out_smt)
import           Induction.Induction   (addLemma, getIndType, nextTimeout,
                                        printResult, printStr, runProver)
import           Induction.Types       (IndState (..), Induction (..), TP (..),
                                        getInduction,
                                        getProver)
import           Parser.Params         (InputFile (..), Params (..),
                                        TheoremProver (..), TipSpec(..), parseParams)
import           Process               (readTheory, run_process, run_process')
import           Prover                (Prover (..), eprover, z3)
import           Utils                 (mcase)

main :: IO()
main = join $ (\(a,b) -> mapM_ (\b' -> runMain a{inputFile = SMT  b'} b') b) <$> preProcess

preProcess :: IO (Params,[FilePath])
preProcess = do

    -- parse input parameters: inout file and verbosity flags
    params <- parseParams

    when (outputLevel params >= 4) $ putStrLn $ show params

    -- Check where to put file
    let filePath = (case tipspec params of
            No              -> out_path ""
            Yes f           -> f ++ "/"
            UseExisting f   -> f ++ "/")
    
    -- check file extension
    (,) params <$> checkInputFile (out_smt) (inputFile params)

runMain :: Params -> FilePath -> IO ()
runMain params preQS = do

    start <- getCurrentTime      
    
    theoryNat <- head . freshPass (runPasses [SortsToNat]) <$> (readTheory preQS)     

    writeFile "./tmp/tmpTheory.smt2" $ show $ ppTheory [] theoryNat 

    runTipSpec params "./tmp/tmpTheory.smt2" --preQS 

    --theory <- readTheory preQS
    
    -- theory exploration
    runTipSpec params preQS

    -- parsing tip qith quickspec to theory
    theory_qs <- readTheory prop_file

    let theory' = theory_qs

    -- TODO better error handling
    catch (runStateT 
                (runTP $ induct (renameLemmas theory'))
                (initState params)
           >> return ())
            (\e -> putStrLn $ "It worked catching error!! Yeahoo" ++ (show (e :: SomeException)))


    end <- getCurrentTime
    let diff = diffUTCTime end start

    when (outputLevel params > 0) $ do
        putStrLn ""
        putStrLn $  "Time taken: " ++ (show diff) ++ "sec"
    return ()
        where -- count the number of conjectures in the theory
            numConj th = length $ fst $ theoryGoals th
            -- induction loop
            induct theory = do
                pstr <- show <$> getProver
                printStr 4 pstr
                th <- loop_conj theory 0 (numConj theory) False
                printResult th
            runTP (TP a) = a

{- Run tipspec depending on choosen parameters.
    | If 'No' then don't run tipspec and simply use the given file
    | If 'Yes' FOLDER then run tipspec while storing the genereated lemma file in the specified folder. Overwrites any existing lemma file in the folder.
    | If 'UseExisting' FOLDER then look for an existing lemma file in FOLDER. If no file is found then run tipspec while storing the genereated lemma file in the sppecified folder.

-}
runTipSpec :: Params -> FilePath -> IO ()
runTipSpec params file = do
    putStrLn $ "Working with file: " ++ file

    -- separate path and name of input file
    let path = takeDirectory file
    let name = takeBaseName file

    -- check params regarding tipspec
    case tipspec params of

        -- don't run tipspec
        No  -> readFile file >>= writeFile prop_file

        -- run tipspec normaly, storing the generated lemmas in a file
        Yes folder -> do

                -- create path to lemma file and folder
                let folder'     = normalise $ path ++ folder
                    lemmaFile   = normalise $ folder' ++ "/" ++ name ++ ".smt2"
                when (outputLevel params > 3) $ 
                    putStrLn $ "New TipSpec file @ " ++ lemmaFile

                -- create folder if not exist, 
                -- the new file will be put in there
                createDirectoryIfMissing False folder'

                -- run tipspec, either quiet or not
                smt <- if outputLevel params > 1
                        then run_process  "tip-spec" "." [file]
                        else run_process' "tip-spec" "." [file,"2>","/dev/null"]
                -- write files
                writeFile lemmaFile smt
                writeFile prop_file smt

        -- look for an existing file, 
        -- if it doesn't exist then run tipspec normally
        UseExisting folder -> do

            -- create path to lemma file and folder
            let folder'     = normalise $ path ++ folder
                lemmaFile   = normalise $ folder' ++ "/" ++ name ++ ".smt2"
            when (outputLevel params > 3) $
                putStrLn $ "Lookin for TipSpec file @ " ++ lemmaFile

            -- if file exist, use it, otherwise generate new file
            mcase (doesFileExist lemmaFile)
                (readFile lemmaFile >>= writeFile prop_file)
                (runTipSpec params{tipspec=Yes folder} file)

-- create the initial state from the given parameters
initState :: Name a => Params -> IndState a
initState par = IndState par
    (selectProver par)
    (getIndType par)
    [] Nothing []

-- When the input file is in haskell we need to run tip-ghc
-- to convert it to smt2
checkInputFile :: FilePath -> InputFile -> IO [FilePath]
checkInputFile fp (HS f)       = do
    putStrLn $ "making smt out of haskell-file : " ++ f
    -- start a external Process for tip-ghc, translating a haskell file
    -- into smt2 (tip-format).
    let (path, file) = splitFileName f
    tip_string <- run_process "tip-ghc" path [file]
    let fileName = (fp ++ (takeBaseName file) ++ ".smt")
    writeFile fileName tip_string
    return [fileName]
checkInputFile fp (SMT f)          = do 
    let fileName = fp ++ (takeFileName f)
    writeFile fileName <$> readFile f
    return [fileName]
checkInputFile fp (Unrecognized f) = do
    files <- getDirectoryContents f
    putStrLn $ "FILENAME " ++ f ++ (head $ files)
    concat <$> mapM getFiles files
        where
           getFiles file = 
             case snd $ splitExtension file of
                ".smt2" -> return [file]
                ".smt"  -> return [file]
                ".hs"   -> checkInputFile fp $ HS (f ++ file)
                _       -> return []


selectProver ::Name a => Params -> Prover a
selectProver p = case backend p of
                    E -> eprover
                    Z -> z3

-- Looping through all conjectures and try to prove them
-- If provable with structural induction, they are put into the theory as proven lemmas
-- If during one loop none is being proved, do not continue
loop_conj :: Name a => Theory a -> Int -> Int -> Bool -> TP a (Theory a)
loop_conj theory curr num continue
    | curr >= num = -- tested all conjectures
        if continue -- check whether to loop again
            then loop_conj theory 0 num False   -- continue
            else mcase (nextTimeout)
                    (loop_conj theory 0 num False)
                    (return theory)       -- done
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
