module Main where

import           Control.Monad.State   (get, join, liftIO, modify, runStateT, filterM,
                                        when, put)
import           Control.Exception     (Exception, SomeException, catch, throw)
import           Control.Monad.Error   (catchError)
import           Data.Maybe            (fromMaybe)
import           Data.List             (find)
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
import           Tip.Formula           (getFormula)
import           Tip.Fresh             (Name, freshPass)
import           Tip.Mod               (renameLemmas)
import           Tip.Passes            (StandardPass(..),provedConjecture, selectConjecture, runPasses, freshPass)
import           Tip.Types             (Theory, fm_attrs)
import qualified Tip.Pretty.SMT.Mod as SMT (ppTheory)

import           Constants             (out_path, prop_file, tip_file, out_smt)
import           Induction.Induction   (addLemma, getIndType, nextTimeout,
                                        printResult, printStr, runProver)
import           Induction.Types       (IndState (..), Induction (..), TP (..), Lemma(..),
                                        getInduction,
                                        getProver,
                                        getLemmas)
import           Parser.Params         (InputFile (..), Params (..),
                                        TheoremProver (..), TipSpec(..), parseParams)
import           Process               (readTheory, run_process, run_process')
import           Prover                (Prover (..), eprover, z3)
import           Utils                 (mcase, subsets)
import           Benchmarks            (writeResult, writeInterrupt)

import                     System.Exit

main :: IO()
main = do
    let mrun = \a c -> do
            m <- runMain a{inputFile = SMT  c} c
            return (m , c)
    listError <- join $ (\(a,b) -> mapM (mrun a) b) <$> preProcess
    mapM_ (\(e,file) -> 
        case e of
            Nothing -> return ()
            Just er -> putStrLn $ "Error in File -- " ++ file ++ " -- " ++ er) (listError)
    return ()

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
    (,) params <$> checkInputFile params (out_smt) (inputFile params)

runMain :: Params -> FilePath -> IO (Maybe String)
runMain params preQS = do

    start <- getCurrentTime      
    
    catch ( do
        --theoryNat <- head . freshPass (runPasses [SortsToNat]) <$> (readTheory preQS)     

        --writeFile "./tmp/tmpTheory.smt2" $ show $ SMT.ppTheory [] theoryNat 

        --runTipSpec params "./tmp/tmpTheory.smt2" --preQS 

        --theory <- readTheory preQS
        
        -- theory exploration
        runTipSpec params preQS

        -- parsing tip qith quickspec to theory
        theory_qs <- readTheory prop_file

        let theory' = theory_qs

        -- TODO better error handling
        runStateT 
            (runTP $ induct (renameLemmas theory') start)
            (initState params)

        return Nothing)
            (\e -> do
            putStrLn $ ">>>>>>>>>>>>> ERROR : " ++ (show (e :: SomeException))
            return $ Just (show (e :: SomeException))
            )


    end <- getCurrentTime
    let diff = diffUTCTime end start

    when (outputLevel params > 0) $ do
        putStrLn ""
        putStrLn $  "Time taken: " ++ (show diff) ++ "sec"
    return Nothing
        where -- count the number of conjectures in the theory
            numConj th = length $ fst $ theoryGoals th
            -- induction loop
            induct theory start = do
                pstr <- show <$> getProver
                printStr 4 pstr
                th <- loop_conj theory 0 (numConj theory) False
                printResult th
                end <- liftIO getCurrentTime
                let diff = diffUTCTime end start
                when (benchmarks params) $ writeResult (show diff) preQS
            runTP (TP a) = a

{- Run tipspec depending on choosen parameters.
    | If 'No' then don't run tipspec and simply use the given file
    | If 'Yes' FOLDER then run tipspec while storing the genereated lemma file in the specified folder. Overwrites any existing lemma file in the folder.
    | If 'UseExisting' FOLDER then look for an existing lemma file in FOLDER. If no file is found then run tipspec while storing the genereated lemma file in the sppecified folder.

-}
runTipSpec :: Params -> FilePath -> IO ()
runTipSpec params file = do
    debug params $ "Working with file: " ++ file

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
                    debug params $ "New TipSpec file @ " ++ lemmaFile

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
            debug params $ "Lookin for TipSpec file @ " ++ lemmaFile

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
checkInputFile :: Params -> FilePath -> InputFile -> IO [FilePath]
checkInputFile params fp (HS f)       = do
    debug params $ "Making smt out of haskell-file : " ++ f
    -- start a external Process for tip-ghc, translating a haskell file
    -- into smt2 (tip-format).
    let (path, file) = splitFileName f
    tip_string <- run_process "tip-ghc" path [file]
    let fileName = (fp ++ (takeBaseName file) ++ ".smt")
    writeFile fileName tip_string
    return [fileName]
checkInputFile _ fp (SMT f)          = do 
    let fileName = fp ++ (takeFileName f)
    writeFile fileName <$> readFile f
    return [fileName]
checkInputFile params fp (Unrecognized f) = do
    files <- getDirectoryContents f
    debug params $ "FILENAME " ++ f ++ (head $ files)
    concat <$> mapM getFiles files
        where
           getFiles file = 
             case snd $ splitExtension file of
                ".smt2" -> return [file]
                ".smt"  -> return [file]
                ".hs"   -> checkInputFile params fp $ HS (f ++ file)
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
    | otherwise  = do
        state <- get
        params <- params <$> get 
        lemmas <- getLemmas       
        (the,state) <- liftIO $ catch (runStateT (runTP $ -- test next conjecture 
            let
                -- pick a conjecture
                th      = selectConjecture curr theory
                -- the formula matching the current conjecture
                formula = head . fst $ theoryGoals th
                -- lookup up unique name of formual
                f_name  = fromMaybe "no name"  $ join $ lookup "name" (fm_attrs formula)
                f_source = join $ lookup "source" (fm_attrs formula)
                f_s = case f_source of
                    Nothing -> f_name
                    Just a -> a
                -- turn formula into printable form
                formulaPrint = getFormula formula
            in do
                liftIO $ removeContentInFolder (out_path "")
                nbrVar  <- inductionSize <$> getInduction
                -- clean temporary state
                modify (\s -> s{axioms = [], ind=Nothing})
                mcase (prove th) -- Test if solvable without induction
                    (do -- Proved without induction
                        printStr 3 $ "| " ++ f_s  ++  "  -- P | " ++ formulaPrint
                        addLemma f_name formulaPrint f_source-- add formula to proved lemmas
                        -- go to next conjecture
                        loop_conj (provedConjecture curr theory) curr (num-1) True)
                    (do
                        let indVars = subsets (nbrInduct params) (nbrVar th)
                        printStr 4 $ unlines ["", "= Indices to induct on =", " " ++ show indVars,""]
                        mcase ( loop_ind th indVars ) -- Attempt induction
                            (do -- Proved using induction
                                --fail "hello"
                                printStr 3 $ "| " ++ f_s  ++  "  -- I | " ++ formulaPrint
                                --fail $ "printout"
                                addLemma f_name formulaPrint f_source-- add formula to proved lemmas
                                -- -- go to next conjecture
                                loop_conj (provedConjecture curr theory) curr (num-1) True)
                            -- Unable to prove with current theory
                            -- try next conjecture
                            (do

                                printStr 3 $ "| " ++ f_s  ++  "  -- U | " ++ formulaPrint
                                loop_conj theory (curr + 1) num continue))
                ) (state) 
            ) (\e -> do 
                    when (show (e :: SomeException) == "user interrupt") ( do
                        --putStrLn $ "User interruption! -- " ++ (show (e :: SomeException) )
                        when (benchmarks params) $ writeInterrupt lemmas "error"
                        fail "^C pressed")
                    fail (show e) )
        put state
        return the
    where runTP (TP a) = a

-- Trying to prove a conjecture, looping over all variables in the conjecture
loop_ind :: Name a => Theory a -> [[Int]] -> TP a Bool
loop_ind theory [] = return False -- tested all variables, unable to prove
loop_ind theory (x:xs) = do

        printStr 4 $ unwords [" Induction on indices", show x ]
        --prepare theory for induction on variables/application in x 
        indPass <- inductionPass <$> getInduction
        let ind_theory = freshPass (indPass x) theory

        prep <- prepare <$> getProver
        liftIO $ printTheories prep ind_theory 0 (out_path ("Theory" ++ show x))
        --(return . show . ppTheory' . head . tff [SkolemiseConjecture])

        printStr 3 "-----------------------------------------"
        mcase (proveAll ind_theory)     -- try induction 
            (do -- proves using induction on 'num'
                modify (\s -> s{ind = Just x}) -- add variables used
                return True)
            (loop_ind theory xs)   -- unable to prove, try next variable


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

talk :: Int -> Params -> String -> IO ()
talk i p = when (outputLevel p >= i) .
                putStrLn

debug :: Params -> String -> IO ()
debug = talk 4

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
        (proved, ax) <- liftIO =<< (parseOut <$> getProver) <*> pure [prob, ep]
        
        lemmas <- getLemmas
        ax' <- mapM (\ln -> case find (\(Lemma n _ _ _ _) -> n==ln) lemmas of
                        Nothing -> return ln --fail "WTF!!!"
                        Just l -> return $ case lemmaSource l of
                                    Nothing -> ln
                                    Just s -> s) ax

        -- add auxilliary lemmas to temporary state
        when proved $ modify (\s -> s{axioms = axioms s ++ ax'})
        return proved
