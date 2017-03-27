module Benchmarks where

import           System.FilePath.Posix  (takeFileName)
import           System.Directory       (createDirectoryIfMissing)
import           Induction.Types        (TP,getLemmas, hLemmas)
import           Control.Monad.State    (liftIO,get)
import           Data.Time              (getCurrentTime, showGregorian
                                         , formatTime, defaultTimeLocale, iso8601DateFormat)
import           Tip.Fresh              (Name)
import           Data.List              (nub)
import           Parser.Params          (Params(..))
import           Induction.Types        (params,Lemma(..))

data Name a => Result a = Result
  {  lemmas :: [Lemma]
  ,  time   :: String
  }

writeResult :: Name a => String -> FilePath -> TP a ()
writeResult time fp = do
    dateU <- liftIO $ getCurrentTime 
    let date = formatTime defaultTimeLocale (iso8601DateFormat Nothing) dateU
    params <- params <$> get
    let benchFolder = "./benchmarks"
    let indFolder = benchFolder ++ "/" ++ (show (indType params)) 
    let proverFolder = indFolder ++ "/" ++ (show (backend params))
    let folder = proverFolder ++ "/"++ date
    let file = folder ++ "/" ++ (takeFileName fp)  

    --fail ("Date: " ++ (showGregorian (utctDay date)))
   -- liftIO $ createDirectoryIfMissing False benchFolder
   -- liftIO $ createDirectoryIfMissing False indFolder
    --liftIO $ createDirectoryIfMissing False proverFolder
    liftIO $ createDirectoryIfMissing True folder


    proved <- getLemmas

    let provedNub = map (\l -> l{hLemmas = nub $ hLemmas l}) proved     

    let res = show (time, provedNub)

    liftIO $ writeFile file res


writeInterrupt :: [Lemma] -> FilePath -> IO ()
writeInterrupt lemmas file = do
    let folder = "./benchmarks/Interrupted"
    let file1 = folder ++ "/" ++ file

    createDirectoryIfMissing True folder

    let provedNub = map (\l -> l{hLemmas = nub $ hLemmas l}) lemmas     
    let res = show provedNub

    writeFile file1 res


     

