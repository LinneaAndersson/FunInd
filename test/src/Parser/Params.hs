module Parser.Params where

import           Options.Applicative   (Parser (..), auto, execParser, flag, str,
                                        flag', fullDesc, header, help, helper,
                                        info, long, metavar, option, progDesc,
                                        short, showDefault, strArgument, value,
                                        (<>), (<|>))
import           Options.Applicative.Builder (subparser, strOption, command, internal)
import           System.FilePath.Posix (splitExtension)

import Parser.Mod

-- type for input parameters
data Params = Params
    { inputFile   :: InputFile
    , outputLevel :: OutputLevel
    , indType     :: IndType
    , timeouts    :: [Int]
    , tipspec     :: TipSpec
    , backend     :: TheoremProver
    , splitCases  :: Bool
    , benchmarks  :: Bool
    , nbrInduct   :: Int
    , stepLemmas :: [String]
    }

instance Show Params where
    show p = unlines
        ["== Program Parameters ==",
         " Input File: "         ++ show (inputFile p),
         " Verbosity Level: "    ++ show (outputLevel p),
         " Induction Type: "     ++ show (indType p),
         " Prover Timeouts: "    ++ show (timeouts p),
         " TipSpec Enabled: "    ++ show (tipspec p),
         " Prover Backend: "     ++ show (backend p),
         " Split Cases: "        ++ show (splitCases p),
         " Run benchmarks: "     ++ show (benchmarks p),
         " #Induction vars: "    ++ show (nbrInduct p),
         " Interactive Lemmas: " ++ show (stepLemmas p)
        ]

data TipSpec = No | Yes String | UseExisting String deriving Show

data TheoremProver = E | Z
    deriving (Show, Eq)

-- verbosity level
type OutputLevel = Int

-- inputfile format
data InputFile = HS FilePath | SMT FilePath | Unrecognized FilePath

instance Show InputFile where
    show (Unrecognized fp)  = fp
    show (HS fp)            = fp
    show (SMT fp)           = fp

data IndType = Structural | Applicative
    deriving Show

type Timeouts = [Int]


-- parse all input parameters
parseParams :: IO Params
parseParams = execParser $ info
                (helper <*>
                    (Params    <$>  parseInputFile
                               <*>  parseOutputLevel
                               <*>  parseIndType
                               <*>  parseProverTimeouts
                               <*>  parseTipSpecEnabled
                               <*>  parseProver
                               <*>  parseSplit
                               <*>  parseBench
                               <*>  parseInductNbr
                               <*>  parseInteract))
                (fullDesc <>
                 progDesc "Prov properties of recursively defined functions" <>
                 -- TODO we really need a better name... :)
                 header "test - Inductive theorem prover")

parseInteract :: Parser [String]
parseInteract = map (("lemma" ++) . show) <$> lemmaNbrs
    where lemmaNbrs = option auto (long "interactive-lemmas" <> metavar "[Int,...]" 
                        <> help "Lemmas to pause at, allowing the user to step through the induction"
                        <> showDefault
                        <> value []) :: Parser [Int]

parseInductNbr :: Parser Int
parseInductNbr = option auto (long "nbr-indvar" <> metavar "INT" <> help "Max number of induction variables"
                        <> showDefault
                        <> value 1)

parseBench :: Parser Bool
parseBench = flag False True (long "benchmark" <> help "Save result of benchmark(s) (default false)")

parseSplit :: Parser Bool
parseSplit =
        flag' False     (long "no-split-cases" <> help "Prove whole property at once")
    <|> flag  True True (long "split-cases" <> help "Split properties by pattern matching (default)")

parseProver :: Parser TheoremProver
parseProver =
        flag' Z     (long "prover-Z3" <> help "Use Z3 as the prover backend")
    <|> flag  E E    (long "prover-E" <> help "Use E-prover as the prover backend (defualt)")

parseProverTimeouts :: Parser [Int]
parseProverTimeouts =
    option auto (long "timeouts" <> metavar "[INT,...]" <> help "Timeouts for the prover"
            <> showDefault
            <> value [1,5,10])

parseTipSpecEnabled :: Parser TipSpec
parseTipSpecEnabled =
        (flag' (const No)
            (long "no-speculate-lemmas"
                  <> help "Attempt proof without theory exploration")
    <|> optFlag' UseExisting
        (long "use-lemmas"
            <> metavar "[FOLDER | ]"
            <> help "Attempt proof after theory exploration, looking for and storing generated lemmas in the specified folder (default)")
    <|> optFlag UseExisting Yes
            (long "speculate-lemmas"
                <> metavar "[FOLDER | ]"
                <> help "Attempt proof after theory exploration"))
    <*> (strArgument (internal <> value "/Generated"))

-- parse vebosity flag
parseOutputLevel :: Parser OutputLevel
parseOutputLevel =
        flag'    4      (long "debug" <> internal)
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
                ".smt" -> SMT fileName
                ".hs"   -> HS  fileName
                _       -> Unrecognized fileName

parseIndType :: Parser IndType
parseIndType =
    flag'    Structural      (long "structural-induction" <> help "Prove using structural induction")
    <|> flag Applicative Applicative  (long "applicative-induction" <> help "Prove using applicative induction (default)")
