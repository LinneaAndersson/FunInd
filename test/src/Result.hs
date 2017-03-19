module Result where

import System.Environment
import Data.List

data Lemma = Lemma
    { lemmaName :: String
    , hLemmas   :: [String]
    , indVar    :: Maybe Int
    } deriving (Show,Read)

compareResult :: FilePath -> FilePath -> IO ()
compareResult file1 file2 = do
    fileContent1 <- readFile file1
    let lemmasF1 = map lemmaName (read fileContent1 :: [Lemma])
    fileContent2 <- readFile file2
    let lemmasF2 = map lemmaName (read fileContent2 :: [Lemma])

    putStrLn ""
    putStrLn $ "----- " ++ file1 ++ " ----" 
    putStrLn $ "Solved: " ++ show (length lemmasF1) ++ " lemmas"
    
    let notSolved1 = filter (\l -> (not (l `elem` lemmasF1))) lemmasF2
    putStrLn $" --- could not solve (" ++ show (length notSolved1) ++ ") lemmas the other file solved: "
    mapM_ putStrLn notSolved1

    putStrLn "-------------------------"

    putStrLn ""
    putStrLn $ "----- " ++ file2 ++ " ----" 
    putStrLn $ "Solved: " ++ show (length lemmasF2) ++ " lemmas"
    let notSolved2 = filter (\l -> (not (l `elem` lemmasF2))) lemmasF1
    putStrLn $" --- could not solve (" ++ show (length notSolved2) ++ ") lemmas the other file solved: "
    mapM_ putStrLn notSolved2

    putStrLn "-------------------------"

    putStrLn ""
    let notSolved = intersect lemmasF1 lemmasF2
    putStrLn $ "Both solved (" ++ show (length notSolved) ++ ") lemmas: "
    mapM_ putStrLn notSolved

getLemma :: FilePath -> String -> IO ()
getLemma file lemma = do
    fileContent <- readFile file
    let lemmas = (read fileContent :: [Lemma])
    case find ((==) lemma . lemmaName) (lemmas) of
        Nothing -> putStrLn "Could not find lemma"
        Just a -> putStrLn $ prettyLemma a


prettyLemma :: Lemma -> String
prettyLemma (Lemma name helpLemmas i) = name 
