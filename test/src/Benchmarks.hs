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
import           Induction.Types        (params)


writeResult :: Name a => FilePath -> TP a ()
writeResult fp = do
    dateU <- liftIO $ getCurrentTime 
    let date = formatTime defaultTimeLocale (iso8601DateFormat Nothing) dateU
    params <- params <$> get
    let benchFolder = "./benchmarks"
    let indFolder = benchFolder ++ "/" ++ (show (indType params)) 
    let proverFolder = indFolder ++ "/" ++ (show (backend params))
    let folder = proverFolder ++ "/"++ date
    let file = folder ++ "/" ++ (takeFileName fp)  

    --fail ("Date: " ++ (showGregorian (utctDay date)))
    liftIO $ createDirectoryIfMissing False benchFolder
    liftIO $ createDirectoryIfMissing False indFolder
    liftIO $ createDirectoryIfMissing False proverFolder
    liftIO $ createDirectoryIfMissing False folder


    proved <- getLemmas

    let provedNub = map (\l -> l{hLemmas = nub $ hLemmas l}) proved     

    let res = show provedNub

    liftIO $ writeFile file res




