module Prover where

import Process
import qualified Jukebox.Provers.E as Ep
--import Jukebox.TPTP.Parse
import Jukebox.Form
import Jukebox.TPTP.Parse


type Flag = String

data Prover = P {
        name  :: FilePath,
        flags :: [Flag],
        prepare :: [String] -> IO String,
        parseOut :: [String] -> IO Bool
    }

instance Show Prover where
    show p = "Prover: " ++ name p ++ ", flags: " ++ (unwords $ flags p)

eprover :: Prover
eprover = P {name = "eprover",
             flags = ["--tstp-in", "--auto",
                        "--silent", "--soft-cpu-limit=5"],
             prepare = jukebox_hs . head,
             parseOut = pout}
    where
        pout :: [String] -> IO Bool
        pout [prob, ep] = do
            prob' <- parseString prob
            case Ep.extractAnswer prob' ep of
                    -- a problem occured
                    Right terms -> do
                                    print "Could not extract the answer from the prover"
                                    return False
                    {-
                        Check the answer. The E prover attempts to prove negations, thus
                        a problem should NOT be 'Satisfiable'.
                    -}
                    Left ans -> case ans of
                        Satisfiable     -> return False
                        Unsatisfiable   -> return True
                        NoAnswer reason ->
                            -- print $ "Could not find the answer:  " ++ (show reason)
                            return False
