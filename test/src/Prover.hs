module Prover where

import           Data.List          (isInfixOf)
import qualified Jukebox.Provers.E  as Ep (extractAnswer)
import           Jukebox.Form       (Answer (..))
import           Jukebox.TPTP.Parse (parseString)
import           Text.Regex         (mkRegex, splitRegex)

import           Tip.Mod            (ppTheory', tff)
import           Tip.Passes         (StandardPass (..),runPasses, freshPass)
import           Tip.Types          (Theory)
import           Tip.Fresh          (Name)

import           Constants          (out_path)
import           Process            (jukebox_hs)
import           Tip.Pretty.SMT as SMT    (ppTheory)

type Flag = String

-- represenatation of a prover
data Name a => Prover a = P {
        -- path to execteable
        name     :: FilePath,
        -- flags given to prover when calling it
        flags    :: [Flag],
        -- function to prepare the theory
        prepare  :: Theory a -> IO String,
        -- parse the output from the prover
        parseOut :: [String] -> IO (Bool, [String]),
        -- timeout
        setTime  :: Int -> Flag
    }

instance Name a => Show (Prover a) where
    show p = unlines
            ["== Prover Information ==",
             "Prover: " ++ name p,
             "flags: " ++ unwords (flags p)
            ]

-- A prover instance of the first-order-logic prover E
eprover :: Name a => Prover a
eprover = P {name = "eproof",
             flags = ["--tstp-in", "--auto", "--full-deriv"],
             prepare = \i ->
                    do
                        let str = show . ppTheory' . head . tff [SkolemiseConjecture] $ i
                        writeFile (out_path "preJukebox") str
                        jukebox_hs str,
             parseOut = pout,
             setTime = \i -> "--soft-cpu-limit=" ++ show i}
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


-- A prover instance of the first-order-logic prover E
z3 :: Name a => Prover a
z3 = P {name = "z3",
        flags = [],
        prepare = \i ->
            do
                let str = show . SMT.ppTheory [] . head . freshPass (z3PrePasses) $ i
                writeFile (out_path "prepared") str
                return str,
        parseOut = pout,
        setTime = \i -> "-T:"++ show i}
    where
        z3PrePasses = runPasses [TypeSkolemConjecture, 
            Monomorphise  False, SimplifyGently, LambdaLift, 
            AxiomatizeLambdas, Monomorphise False, SimplifyGently, CollapseEqual,
            RemoveAliases, SimplifyGently , AxiomatizeFuncdefs2, RemoveMatch, SkolemiseConjecture, NegateConjecture] 
        pout :: [String] -> IO (Bool,[String])
        pout [prob, out] = do
            if "unsat" `isInfixOf` out 
                then return (True, output out) 
                else return (False, [])



