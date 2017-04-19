module Main where

import           Control.Monad.State   (join, liftIO, runStateT, when)
import           Control.Exception     (Exception, SomeException, catch)
import           System.FilePath.Posix (splitFileName,
                                        takeBaseName,
                                        splitExtension,
                                        takeFileName)
import           System.Directory      (getDirectoryContents)
import           Data.Time             (getCurrentTime,diffUTCTime)

import           Tip.Core              (theoryGoals,Builtin(..),Head(..),Expr(..))
import           Tip.Fresh             (Name,freshPass)
import           Tip.Mod               (renameLemmas,universeBi)
import           Tip.Pretty.SMT as SMT
import           Tip.Passes (StandardPass(..), runPasses)
import           Tip.Passes.ReplacePrelude (replacePrelude)

import           Constants             (out_path, prop_file, out_smt)
import           Induction.Induction   (getIndType, printResult, printStr)
import           Induction.Proof       (loop_conj)
import           Induction.Types       (IndState (..), TP (..), getProver)

import           Parser.Params         (InputFile (..), Params (..),
                                        TheoremProver (..), TipSpec(..), parseParams)
import           IO.Output             (debug, runTipSpec)
import           IO.Process            (readTheory, run_process)

import           Prover                (Prover (..), eprover, z3)
import           Benchmarks            (writeResult)


main :: IO()
main = do

    -- Function to run the prover on one file at a time, 
    -- attempting to prove all conjectures
    let mrun a c = do
            m <- runMain a{inputFile = SMT  c} c
            return (m , c)

    -- Run the prover and collect errors, if any
    listError <- join $ (\(a,b) -> mapM (mrun a) b) <$> preProcess

    -- Check for errors
    mapM_   (\(err, file) -> 
                case err of
                    Nothing -> return ()
                    Just er -> putStrLn $ "Error in File -- " ++ file ++ " -- " ++ er)
            listError
    return ()

preProcess :: IO (Params,[FilePath])
preProcess = do

    -- parse input parameters: inout file and verbosity flags
    params <- parseParams

    debug params $ show params

    -- Check where to put file
    let filePath =
            case tipspec params of
                No              -> out_path ""
                Yes f           -> f ++ "/"
                UseExisting f   -> f ++ "/"
    
    -- Check file extension
    (,) params <$> checkInputFile params out_smt (inputFile params)

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

        let theory' = if (backend params) == E then head $ freshPass (runPasses [IntToNat, SortsToNat]) $ freshPass (replacePrelude) theory_qs else theory_qs

       
        -- TODO better error handling
        runStateT 
            (runTP $ induct (renameLemmas theory') start)
            (initState params)

        return Nothing)
            (\e -> do
            putStrLn $ ">>>>>>>>>>>>> ERROR : " ++ show (e :: SomeException)
            return $ Just (show (e :: SomeException))
            )

    -- calculate finishing time
    end <- getCurrentTime
    let diff = diffUTCTime end start

    -- print time taken
    when (outputLevel params > 0) $ do
        putStrLn ""
        putStrLn $  "Time taken: " ++ show diff ++ "sec"
    return Nothing
        where -- count the number of conjectures in the theory
            numConj th = length $ fst $ theoryGoals th
            -- induction loop
            induct theory start = do

                -- print prover if debug mode
                pstr <- show <$> getProver
                printStr 4 pstr

                -- loop through all conjectures and attempt
                -- to prove them
                th <- loop_conj theory 0 (numConj theory) False

                printResult th

                -- calculate finishing time
                end <- liftIO getCurrentTime
                let diff = diffUTCTime end start

                -- write benchmarks
                when (benchmarks params) $ writeResult (show diff) preQS
            runTP (TP a) = a

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
    let fileName = fp ++ takeBaseName file ++ ".smt"
    writeFile fileName tip_string
    return [fileName]
checkInputFile _ fp (SMT f)          = do 
    let fileName = fp ++ takeFileName f
    writeFile fileName <$> readFile f
    return [fileName]
checkInputFile params fp (Unrecognized f) = do
    files <- getDirectoryContents f
    debug params $ "FILENAME " ++ f ++ head files
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

