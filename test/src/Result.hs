module Result where

import System.Environment
import Data.List (filter, find, intersect, groupBy, sort, intersperse, replicate)
import System.FilePath (takeBaseName, equalFilePath)
import System.Directory (getDirectoryContents, doesFileExist, doesDirectoryExist)
import Control.Monad (filterM)
import Data.Maybe (isJust)
import Numeric (showFFloat)

data Lemma = Lemma
    { lemmaName   :: String
    , lemmaSource :: Maybe String
    , hLemmas     :: [String]
    , indVar      :: Maybe [Int]
    , formula     :: String
    } deriving (Show,Read)

data ResultFile = ResultFile 
    { indName   :: String
    , prover    :: String
    , fileName  :: String
    , path      :: FilePath
    } deriving (Show, Eq)

instance Ord ResultFile where
    a <= b = fileName a <= fileName b

data Result = Result 
    { fileNames :: [String]
    , results   :: [(String, [(String, Int, Int)])]
    } deriving (Show)

type Date = String

printLatex :: Result -> String
printLatex r = (unlines [start,startTab]) ++ rows ++ (unlines [endTab,end])
    where
        start = "\\begin{table}[H]"
        startTab = "\\begin{tabular}{" ++ replicate (1+length (fileNames r)) 'l' ++ "}"
        names = unwords $ intersperse "&" ("":fileNames r) 
        res = map (\(n,ls) -> unwords $ intersperse "&" (n : map show ls)) (results r)
        rows = unlines $ intersperse "\\\\" (names: res) 
        endTab = "\\end{tabular}"
        end = "\\end{table}"

getTableResult :: [ResultFile] -> IO Result
getTableResult files = do
    -- group the files by induction and prover
    let groupedFiles = groupBy (\f1 f2 -> indName f1 == indName f2 && prover f1 == prover f2) files

    -- find the files that exist in all pairs of induction and prover
    let groupedNames = map (map fileName) groupedFiles
    let files1 = map fileName $ head groupedFiles
    let commonFiles = filter (\f -> and $ map (f `elem`) groupedNames) files1 
    
    -- remove not common files from grouped files
    let common = map (filter (\f -> (fileName f) `elem` commonFiles)) groupedFiles
    
    -- sort by fileName
    let fileNames = sort commonFiles
    let sortedGrouped = map sort common
    results <- mapM getResult sortedGrouped
    return $ Result fileNames results

getResult :: [ResultFile] -> IO (String, [(String, Int, Int)])
getResult rf = do
    -- the name of the induction and prover (one row)
    let name = indName (head rf) ++ ", " ++ prover (head rf)

    ---------- the number of proved conjectures in each file
    -- read all files
    filesContent <- mapM (readFile . path) rf 
    let timeLemmas = map read filesContent :: [(String,[Lemma])]

    let helpF (t,ls) = (prettyTime t,length ls, length $ filter (isJust . indVar) ls)

    return (name,map helpF timeLemmas)
    
prettyTime :: String -> String
prettyTime [] = []    
prettyTime ([a]) = [a] 
prettyTime (a:b:as) 
    | a == '.' = [a,b]
    | otherwise = a : prettyTime (b:as)  

writeResult :: Date -> FilePath -> FilePath -> IO ()
writeResult  date path_out path = do
    --get all induction directories
    dirs <- getDirectories path
    -- create files for all kind of inductions
    files <- concat <$> mapM (\d -> getFilesWithInduction date (takeBaseName d) d) dirs
    -- create nice table with result
    result <- getTableResult files
    -- display table
    putStrLn $ printLatex result
    return ()

getFilesWithInduction :: Date -> String -> FilePath -> IO [ResultFile]
getFilesWithInduction date induction path = do
    dirs <- getDirectories path
    files <- concat <$> mapM (\d -> getFilesWithProver date induction (takeBaseName d) d) dirs
    return files

getFilesWithProver :: Date -> String -> String -> FilePath -> IO [ResultFile]
getFilesWithProver date induction prover path = do
    let pathDate = (path ++ "/" ++ date)
    -- Get all files for specified prover and induction
    files <- getFiles pathDate
    let rFiles = map (\f -> ResultFile induction prover (takeBaseName f) f) files 
    return rFiles 

getDirectories :: FilePath -> IO [FilePath]
getDirectories path = do
    let pathN = path ++ "/" 
    content <- map (\d -> pathN ++ d) <$> getDirectoryContents path
    dirs <- filterM (\d -> do
                        b <- doesDirectoryExist d
                        return $ b && d /= (pathN ++ ".") && d /= (pathN ++ "..") ) content
    return dirs

getFiles :: FilePath -> IO [FilePath]
getFiles path = do
    let pathN = path ++ "/" 
    content <- map (\d -> pathN ++ d) <$> getDirectoryContents path
    files <- filterM doesFileExist content
    return files


compareResult :: FilePath -> FilePath -> IO ()
compareResult file1 file2 = do
    fileContent1 <- readFile file1
    let (time1,lemmas1) = read fileContent1 :: (String,[Lemma])
    let lemmasName1 = map lemmaName lemmas1
    fileContent2 <- readFile file2
    let (time2,lemmas2) = read fileContent2 :: (String,[Lemma])
    let lemmasName2 = map lemmaName lemmas2

    putStrLn ""
    putStrLn $ "----- " ++ file1 ++ " ----" 
    putStrLn $ "Solved: " ++ show (length lemmas1) ++ " lemmas"
    
    let notSolved1 = filter (\l -> (not ((lemmaName l) `elem` lemmasName1))) lemmas2
    putStrLn $" --- could not solve (" ++ show (length notSolved1) ++ ") lemmas the other file solved: "
    mapM_ (putStrLn . prettyLemma) notSolved1

    putStrLn "-------------------------"

    putStrLn ""
    putStrLn $ "----- " ++ file2 ++ " ----" 
    putStrLn $ "Solved: " ++ show (length lemmas2) ++ " lemmas"
    let notSolved2 = filter (\l -> not $ lemmaName l `elem` lemmasName2) lemmas1 
    putStrLn $ " --- could not solve (" ++ (show (length notSolved2)) ++ ") lemmas the other file solved: "
    mapM_ (putStrLn . prettyLemma) notSolved2

    putStrLn "-------------------------"

    putStrLn ""
    let notSolved = filter (\l ->  lemmaName l `elem` intersect lemmasName1 lemmasName2) lemmas1
    putStrLn $ "Both solved (" ++ show (length notSolved) ++ ") lemmas: "
    mapM_ (putStrLn . prettyLemma) notSolved

getLemma :: FilePath -> String -> IO ()
getLemma file lemma = do
    fileContent <- readFile file
    let lemmas = (read fileContent :: [Lemma])
    case find ((==) lemma . lemmaName) (lemmas) of
        Nothing -> putStrLn "Could not find lemma"
        Just a -> putStrLn $ prettyLemma a


prettyLemma :: Lemma -> String
prettyLemma (Lemma name source helpLemmas i formula) = name ++ ": " ++ formula 
