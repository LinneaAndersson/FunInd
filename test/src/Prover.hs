module Prover where

import Process
import qualified Jukebox.Provers.E as Ep
--import Jukebox.TPTP.Parse
import Jukebox.Form
import Jukebox.TPTP.Parse
import Data.List

type Flag = String

data Prover = P {
        name  :: FilePath,
        flags :: [Flag],
        prepare :: [String] -> IO String,
        parseOut :: [String] -> IO (Bool, [String])
    }

instance Show Prover where
    show p = "Prover: " ++ name p ++ ", flags: " ++ (unwords $ flags p)

eprover :: Prover
eprover = P {name = "eproof",
             flags = ["--tstp-in", "--auto",
                        "--soft-cpu-limit=5"],
             prepare = jukebox_hs . head,
             parseOut = pout}
    where
        pout :: [String] -> IO (Bool,[String])
        pout [prob, ep] = do
            prob' <- parseString prob
            case Ep.extractAnswer prob' ep of
                    -- a problem occured
                    Right terms -> do
                                    print "Could not extract the answer from the prover"
                                    return (False,[])
                    {-
                        Check the answer. The E prover attempts to prove negations, thus
                        a problem should NOT be 'Satisfiable'.
                    -}
                    Left ans -> case ans of
                        Satisfiable     -> return (False,[])
                        Unsatisfiable   -> 
                            do  
                                --print $ output ep 
                                return (True,output ep)
                        NoAnswer reason ->
                            -- print $ "Could not find the answer:  " ++ (show reason)
                            return (False,[show reason])


output :: String -> [String] 
output ep = getAxioms $ lines ep

getAxioms :: [String] -> [String]
getAxioms [] = []
getAxioms (x:xs) 
    | isInfixOf "axiom" x = x : (getAxioms xs)
    | otherwise = getAxioms xs 
                

