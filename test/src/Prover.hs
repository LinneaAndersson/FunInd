module Prover where

import           Data.List
import qualified Jukebox.Provers.E  as Ep
--import Jukebox.TPTP.Parse
import           Jukebox.Form
import           Jukebox.TPTP.Parse
import           Process
import           Text.Regex
import           Tip.Mod
import           Tip.Parser
import           Tip.Passes
import           Tip.Types
import           Tip.Pretty
import           Tip.Fresh

import           Constants

type Flag = String
-- represenatation of a prover
data (Name a, PrettyVar a) => Prover a = P {
        -- path to execteable
        name     :: FilePath,
        -- flags given to prover when calling it
        flags    :: [Flag],
        -- function to prepare the theory
        prepare  :: Theory a -> IO String,
        -- parse the output from the prover
        parseOut :: [String] -> IO (Bool, [String])
    }

instance Name a => Show (Prover a) where
    show p = "Prover: " ++ name p ++ ", flags: " ++ unwords (flags p)

-- A prover instance of the first-order-logic prover E
eprover :: Name a => Prover a
eprover = P {name = "eproof",
             flags = ["--tstp-in", "--auto", "--full-deriv",
                        "--soft-cpu-limit=1"],
             prepare = (\i ->
                    do 
                        let str = show . ppTheory' . head . tff [SkolemiseConjecture] $ i
                        writeFile (out_path "preJukebox") str
                        jukebox_hs str),
             parseOut = pout}
    where
        pout :: [String] -> IO (Bool,[String])
        pout [prob, ep] = do
            -- parse into Problem Form
            prob' <- parseString prob
            -- parse the answer
            case Ep.extractAnswer prob' ep of
                    -- a problem occured
                    -- TODO what is terms?
                    Right terms -> do
                                    print "Could not extract the answer from the prover"
                                    return (False,[])
                    {-
                        Check the answer. The E prover attempts to prove negations, thus
                        a problem should NOT be 'Satisfiable', if it is provable.
                    -}
                    Left ans    ->
                        case ans of
                            Satisfiable     -> return (False,[])
                            Unsatisfiable   -> return (True,output ep)
                            NoAnswer reason -> return (False,[show reason])


output :: String -> [String]
output = getAxioms . lines

-- find auxiliary lemmas used in the proof
getAxioms :: [String] -> [String]
getAxioms [] = []
getAxioms (x:xs)
    | "lemma" `isInfixOf` x = filter (/=' ') (sDone (sp x)) : getAxioms xs
    | otherwise             = getAxioms xs
    where -- split on ','
        sp = splitRegex (mkRegex ",")
        -- name of any aux lemma is last in split
        -- init since last char is ')'
        sDone s = init (s !! (length s - 1))
