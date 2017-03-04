module Parser.Params where

import           Options.Applicative
import           System.FilePath.Posix

-- type for input parameters
data Params = Params
    { inputFile   :: InputFile
    , outputLevel :: OutputLevel
    , indType     :: IndType
    , timeouts    :: [Int]
    , tipspec     :: Bool
    }

-- verbosity level
type OutputLevel = Int

-- inputfile format
data InputFile = HS FilePath | SMT FilePath | Unrecognized

data IndType = Structural | Applicative

type Timeouts = [Int]


-- parse all input parameters
parseParams :: IO Params
parseParams = execParser $ info
                (helper <*>
                    (Params    <$>     parseInputFile
                               <*>     parseOutputLevel
                               <*>     parseIndType
                               <*>     parseProverTimeouts
                               <*>     parseTipSpecEnabled))
                (fullDesc <>
                 progDesc "Prov properties of recursively defined functions" <>
                 -- TODO we really need a better name... :)
                 header "test - Inductive theorem prover")

parseProverTimeouts :: Parser [Int]
parseProverTimeouts =
    option auto (long "timeouts" <> metavar "TIMEOUT..." <> help "Timeouts for the prover"
            <> showDefault
            <> value [1,5,10])
    -- <|> (value [5])

parseTipSpecEnabled :: Parser Bool
parseTipSpecEnabled =
        flag'   False       (long "no-speculate-lemmas"
                            <> help "Attempt proof without theory exploration")
    <|> flag    True True   (long "speculate-lemmas"
                            <> help "Attempt proof after theory exploration (default)")

-- parse vebosity flag
parseOutputLevel :: Parser OutputLevel
parseOutputLevel =
        flag'    4      (long "debug")
    <|> flag'    0      (long "quiet"       <> short 'q'    <> help "Only print whether user properties were proven or not")
    <|> flag'    3      (long "full-output" <> short 'f'    <> help "Print full progress of the program as well a summary")
    <|> flag'    2      (long "summary"     <> short 's'    <> help "Print a summary of all proven properties")
    <|> flag     1 1    (long "user"                        <> help "Print summary of proven user specified properties (default)")

-- parse inputfile's path
parseInputFile :: Parser InputFile
parseInputFile = fun <$> strArgument (metavar "FILENAME" <> help "File to process")
    where fun fileName =
            case snd $ splitExtension fileName of
                ".smt2" -> SMT fileName
                ".hs"   -> HS  fileName
                _       -> Unrecognized

parseIndType :: Parser IndType
parseIndType =
    flag'    Structural      (long "structural-induction" <> help "Prove using structural induction")
    <|> flag Applicative Applicative  (long "applicative-induction" <> help "Prove using applicative induction (default)")
