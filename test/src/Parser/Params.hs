module Parser.Params where

import           Options.Applicative
import           System.FilePath.Posix

-- type for input parameters
data Params = Params
    { inputFile   :: InputFile
    , outputLevel :: OutputLevel
    }

-- verbosity level
type OutputLevel = Int

-- inputfile format
data InputFile = HS FilePath | SMT FilePath | Unrecognized

-- parse vebosity flag
parseOutputLevel :: Parser OutputLevel
parseOutputLevel =
        flag'    0      (long "quiet"       <> short 'q'    <> help "Only print whether user properties were proven or not")
    <|> flag'    3      (long "full-output" <> short 'f'    <> help "Print full progress of the program as well as summary")
    <|> flag'    2      (long "summary"     <> short 's'    <> help "Print a summary of all proven properties")
    <|> flag     1 1    (long "user"                        <> help "Print summary of proven user specified properties")

-- parse inputfile's path
parseInputFile :: Parser InputFile
parseInputFile = fun <$> strArgument (metavar "FILENAME" <> help "File to process")
    where fun fileName =
            case snd $ splitExtension fileName of
                ".smt2" -> SMT fileName
                ".hs"   -> HS  fileName
                _       -> Unrecognized
-- parse all input parameters
parseParams :: IO Params
parseParams = execParser $ info
                (helper <*>
                    (Params    <$>     parseInputFile
                               <*>     parseOutputLevel))
                (fullDesc <>
                 progDesc "Proving properties of recursively defined functions" <>
                 header "test - Inductive theorem prover")
