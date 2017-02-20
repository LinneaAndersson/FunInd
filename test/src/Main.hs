
module Main where

import           Control.Monad.State
import           Data.Either
import           Data.List
import           Data.List.Split
import           Data.Maybe
--import GHC.IO.Handle
import           Parser.Params
import           System.Environment
import           System.FilePath.Posix
import           System.IO
import           Text.Regex
import           Tip.Core              (forallView, theoryGoals, free)
import           Tip.Formula
import Tip.Funny.Property (propBody,propFunc,propName,propLocals)
import           Tip.Haskell.Rename
--import           Tip.Haskell.Translate
import           Tip.Lint
import           Tip.Mod
import           Tip.Parser
import           Tip.Passes
import           Tip.Pretty
import           Tip.Pretty.TFF
import           Tip.Scope
import Tip.Rename
import           Tip.Types
import           Tip.Passes.Funny

import           Constants
import           Induction
import           Process
import           Prover                hiding (getAxioms)
import           Utils

main :: IO()
main = do

    -- parse input parameters: inout file and verbosity flags
    params <- parseParams

    -- check file extension
    preQS <- checkInputFile (inputFile params)

    -- created conjectures
    -- TODO verbosity
    prop_string <- run_process "tip-spec" "." [preQS]
    writeFile prop_file prop_string

    -- parsing tip qith quickspec to theory
    theory_qs <- readTheory prop_file

    --printStr ""
    --printStr "---------- Now considering: ----------"

    -- Structural Induction TODO:: ADD (freshPass (monomorphise False) theory_qs)
    case induct (renameLemmas theory_qs) >>= printResult . fst of
        Induct a -> runStateT a $ initState params
    return ()
        where -- count the number of conjectures in the theory
            numConj th = length $ fst $ theoryGoals th
            -- induction loop
            induct theory = loop_conj theory 0 (numConj theory) False
            -- initial state
            initState par = IndState par eprover [] Nothing []

-- When the input file is in haskell we need to run tip-ghc
-- to convert it to smt2
checkInputFile :: InputFile -> IO FilePath
checkInputFile (HS f)      = do
                -- start a external Process for tip-ghc, translating a haskell file
                -- into smt2 (tip-format).
                let (path, file) = splitFileName f
                tip_string <- run_process "tip-ghc" path [file]
                writeFile tip_file tip_string
                return tip_file
checkInputFile (SMT f)      = return f
checkInputFile Unrecognized = fail "Incompatible file extension"



-- Looping through all conjectures and try to prove them
-- If provable with structural induction, they are put into the theory as proven lemmas
-- If during one loop none is being proved, do not continue
-- TODO change timeout on continue
loop_conj :: Theory Id -> Int -> Int -> Bool -> Induction (Theory Id, Bool)
loop_conj theory curr num continue
    | curr >= num = -- tested all conjectures
        if continue -- check whether to loop again
            then loop_conj theory 0 num False   -- continue
            else return (theory, continue)      -- done
    | otherwise  = -- test next conjecture
        let
            -- pick a conjecture
            th0      = selectConjecture curr theory
            -- find all variables
            vars    =  fst . forallView $ fm_body $ head . fst $ theoryGoals th0
            -- count variables, to be used in induction loop
            nbrVar  = length vars
            -- the formula matching the current conjecture
            formula = head . fst $ theoryGoals th0
            -- lookup up unique name of formual
            f_name  = fromMaybe "no name"  $ join $ lookup "name" (fm_attrs formula)
            -- turn formula into printable form
            formulaPrint = showFormula formula
        in do
            liftIO $ putStrLn "------------------------------"
            liftIO $ putStrLn $ show $ ppTheory' th0
            liftIO $ putStrLn "------------------------------"
            --liftIO $ putStrLn . show . ppExpr . func_body . head . thy_funcs $ th
            let     bt = betterTest th0 $ fm_body formula
            let     prop = fst . head $ bt
            let     ps = Formula Assert [] [] $ propBody prop
            let     varDefs = map (\l -> Signature (lcl_name l) [] (PolyType [] [] (lcl_type l))) (propLocals prop)
            let     sigs = Signature (gbl_name (propName prop)) [] (gbl_type (propName prop))
            let     exprs = map (Formula Assert [] (map lcl_name $ propLocals prop) ) $ snd . head $ bt
            let     th = th0{thy_asserts = ps : exprs ++ (thy_asserts th0), thy_sigs = sigs : varDefs ++ (thy_sigs th0) }
            liftIO $ do
                      putStrLn $ show $ ppTheory' th
                      putStrLn "-----------------------------------------"
                      --putStrLn $ "-------free------- " ++ (show $ map (free . fm_body) exprs )
            -- liftIO $ mapM_ (putStrLn . show . ppExpr 0) $
            --liftIO $ mapM_ (putStrLn . show . ppExpr . body) $ test th $ fm_body formula
            --liftIO . putStrLn . show $ map (map snd) . freshIds th . findApps (thy_funcs th) . fm_body . head . fst $ theoryGoals th
            printStr 3 $ "|       | " ++ formulaPrint
            -- clean temporary state
            modify (\s -> s{axioms = [], ind=Nothing})
            mcase (prove th) -- Test if solvable without induction
                (do -- Proved without induction
                    addLemma f_name -- add formula to proved lemmas
                    -- go to next conjecture
                    loop_conj (provedConjecture curr theory) curr (num-1) continue)
                (mcase (loop_ind th 0 nbrVar) -- Attempt induction
                        (do -- Proved using induction
                            addLemma f_name -- add formula to proved lemmas
                            -- -- go to next conjecture
                            loop_conj (provedConjecture curr theory) curr (num-1) True)
                        -- Unable to prove with current theory
                        -- try next conjecture
                        (loop_conj theory (curr + 1) num continue))



-- Trying to prove a conjecture, looping over all variables in the conjecture
loop_ind :: Theory Id -> Int -> Int -> Induction Bool
loop_ind theory num tot
    | num >= tot = return False -- tested all variables, unable to prove
    | otherwise  =
        mcase (proveAll ind_theory)     -- try induction on one variable
            (do -- proves using induction on 'num'
                modify (\s -> s{ind = Just num}) -- add variable used
                return True)
            (loop_ind theory (num+1) tot)   -- unable to prove, try next variable
    where -- prepare theory for induction on variable 'num'
        ind_theory = freshPass (induction [num]) theory

-- Returns true if all conjectures are provable
proveAll :: [Theory Id] -> Induction Bool
proveAll []       = return True
proveAll (th:ths) = mcase (prove th)
                        (proveAll ths)
                        (return False)


-- trying to prove one conjecture, returns true if provable
-- TODO passes should be prover dependent!!!
-- TODO Do something proper about multi-theories
prove :: Theory Id -> Induction Bool
prove th = do
    {-let goal' = head $ passes th -- prepare conjecture using various passes
        goal''  = ppTheory' goal' -- turn conjecture into printable format
    in do -}
        --liftIO $ writeFile (out_path "goal.smt2") $ show $ goal''

        -- retrieve the preparation function from the Prover
        -- and apply it to the goal
        prob <- liftIO =<< (prepare <$> getProver) <*> pure th
        liftIO $ writeFile (out_path "problem") prob

        -- run prover on the problem
        ep <- runProver $ out_path "problem"

        -- check the output from the Prover by using
        -- the Provers parse function
        (b, ax) <- liftIO =<< (parseOut <$> getProver) <*> pure [prob, ep]
        -- add auxilliary lemmas to temporary state
        when b $ modify (\s -> s{axioms = axioms s ++ ax})
        return b
