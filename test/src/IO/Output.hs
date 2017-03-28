module IO.Output where

import Control.Monad.State (when)

import IO.Process (run_process, run_process')

import Parser.Params (Params(..), TipSpec(..)   )

import System.Directory (doesFileExist, createDirectoryIfMissing, createDirectory, removeDirectoryRecursive)
import System.FilePath.Posix (normalise, takeBaseName, takeDirectory)

import Tip.Types (Theory(..))

import Constants (prop_file)
import Utils (mcase)

-- print if outlevel >= given int
talk :: Int -> Params -> String -> IO ()
talk i p = when (outputLevel p >= i) .
                putStrLn

-- print if debug is on
debug :: Params -> String -> IO ()
debug = talk 4

-- write all theories to files
printTheories :: (Theory a -> IO String) -> [Theory a] -> Int -> String -> IO ()
printTheories _ [] _ _          = return ()
printTheories prep (t:ts) i s   =
    do
        createDirectoryIfMissing False s

        -- format and write a theory to file 
        prob <- prep t
        writeFile (s ++ "/problem" ++ show i) prob

        printTheories prep ts (i+1) s

-- recreate a folder
removeContentInFolder :: String -> IO ()
removeContentInFolder s = do
    removeDirectoryRecursive s
    createDirectory s


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
        name = takeBaseName file

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
