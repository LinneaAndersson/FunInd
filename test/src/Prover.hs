{-# LANGUAGE OverloadedStrings #-}

module Prover where

import           Data.List          (isInfixOf)
import           Data.Char          (isNumber)
import qualified Jukebox.Provers.E  as Ep (extractAnswer)
import           Jukebox.Form       (Answer (..))
import           Jukebox.TPTP.Parse (parseString)
import           Text.Regex         (mkRegex, splitRegex)
import           Text.Regex.Applicative (findFirstInfix,string,some, psym, RE(..))
import           Text.Regex.Applicative.Common (digit)

import           Tip.Mod            (ppTheoryTFF, tff)
import           Tip.Passes         (StandardPass (..),runPasses, freshPass)
import           Tip.Types          (Theory)
import           Tip.Fresh          (Name)

import           Constants          (out_path)
import           IO.Process            (jukebox_hs,readTheory)
import qualified Tip.Pretty.SMT as SMT2 (ppTheory)
import qualified Tip.Pretty.SMT.Mod as SMT    (ppTheory)

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
                        let str = show . ppTheoryTFF . head . tff [IntToNat, SortsToNat, SkolemiseConjecture] $ i
                        writeFile (out_path "prepared") str
                        jukebox_hs str,
             parseOut = pout,
             setTime = \i -> unwords ["--cpu-limit=" ++ show (i)]}
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


-- A prover instance for Z3
z3 :: Name a => Prover a
z3 = P {name = "z3",
        flags = ["-smt2","proof=true","unsat-core=true","pp.pretty-proof=true"],
        prepare = \i ->
            do
                writeFile "./tmpout" (show . SMT2.ppTheory [] $ i)
                i' <- readTheory "./tmpout"
                let str = show . SMT.ppTheory [] . head . freshPass (z3PrePasses) $ i'
                writeFile (out_path "prepared") str
                return str,
        parseOut = pout,
        setTime = \i -> "-T:"++ show i}
    where
        z3PrePasses = runPasses [TypeSkolemConjecture, 
            Monomorphise  False, SimplifyGently, LambdaLift, 
            AxiomatizeLambdas, Monomorphise False, SimplifyGently, CollapseEqual,
            RemoveAliases, SimplifyGently , 
            AxiomatizeFuncdefs2, 
            RemoveMatch, SkolemiseConjecture, NegateConjecture] 
        pout :: [String] -> IO (Bool,[String])
        pout [prob, out] = 
            if "unsat" `isInfixOf` out 
                then return (True, output out) 
                else return (False, [])

-- split input by lines and then search for lemmas
output :: String -> [String]
output = getAxioms . lines

-- locate all lemmas in the given list of strings.
getAxioms :: [String]-> [String]
getAxioms [] = []
getAxioms (x:xs) =
    case findFirstInfix (regex) x of
        Nothing         -> getAxioms xs
        Just (_,a,s)    -> ("lemma"++a) : getAxioms (s:xs)
    where regex =  (string "lemma") *>  (some $ psym isNumber) :: RE Char String

