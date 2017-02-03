module Prover where

type Flag = String

data Prover = P {
        name  :: FilePath,
        flags :: [Flag],
        prepare :: IO String
        parseOut :: [String] -> IO Bool
    }

eprover :: Prover
eprover = P {name = "eprover",
             flags = ["--tstp-in", "--auto",
                        "--silent", "--soft-cpu-limit=5"],
             prepare = undefined,
             parseOut = pout}
    where
        pout [prob, ep] =
            case Ep.extractAnswer prob ep of
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
