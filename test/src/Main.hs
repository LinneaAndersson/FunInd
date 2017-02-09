
module Main where

import Control.Monad.State
import Data.List.Split
import Data.List
import Data.Maybe
import Data.Either
--import GHC.IO.Handle
import Parser.Params
import Utils
import Tip.Formula
import Tip.Parser
import Tip.Types
import Tip.Core (theoryGoals, forallView)
import Tip.Passes
import System.IO
import System.Environment
import System.FilePath.Posix
import Text.Regex
import Tip.Haskell.Translate
import Tip.Haskell.Rename
import Tip.Mod
import Tip.Pretty.TFF
import Tip.Scope
import Tip.Pretty
import Tip.Lint
import Induction
import Constants


import Prover hiding (getAxioms)
import Process

main :: IO()
main = do

    params <- parseParams
    preQS <- checkInputFile (inputFile params)

    -- created conjectures
    prop_string <- run_process "tip-spec" "." [preQS]
    writeFile prop_file prop_string

    -- parsing tip qith quickspec to theory
    theory_qs <- readTheory prop_file

    --printStr ""
    --printStr "---------- Now considering: ----------"
    
    -- Structural Induction
    case (induct (renameLemmas theory_qs) >>= (\(th, b) -> printResult th)) of
        Induct a -> runStateT a $ initState params
    return ()
        where -- count the number of conjectures in the theory
            numConj th = length $ fst $ theoryGoals th
            induct theory = loop_conj theory 0 (numConj theory) False
            initState par = IndState par eprover [] Nothing []

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
loop_conj :: Theory Id -> Int -> Int -> Bool -> Induction (Theory Id, Bool)
loop_conj theory curr num continue
    | curr >= num = -- tested all conjectures
        if continue -- check whether to loop again
            then loop_conj theory 0 num False   -- continue
            else return (theory, continue)      -- done
    | otherwise  = -- test next conjecture
        let
            -- pick a conjecture
            th = selectConjecture curr theory
            -- find all variables
            vars =  fst . forallView $ fm_body $ head . fst $ theoryGoals th
            -- count variables, to be used in induction loop
            nbrVar = length vars
            
            formula = head . fst $ theoryGoals th
            f_name = fromMaybe "no name"  $ join $ lookup "name" (fm_attrs formula) 
            formulaPrint = showFormula formula
        in do
            printStr 3 $ "|       | " ++ formulaPrint 
            modify (\s -> s{axioms = [], ind=Nothing})
            mcase (prove th) -- Test if solvable without induction
                (do -- Proved without induction
                    addLemma f_name
                    loop_conj (provedConjecture curr theory) curr (num-1) continue)
                (mcase (loop_ind th 0 nbrVar) -- Attempt induction
                        (do -- Proved using induction
                            addLemma f_name
                            loop_conj (provedConjecture curr theory) curr (num-1) True)
                        (loop_conj theory (curr + 1) num continue)) -- Unable to prove with current theory 



-- Trying to prove a conjecture, looping over all variables in the conjecture
loop_ind :: Theory Id -> Int -> Int -> Induction Bool
loop_ind theory num tot 
    | num >= tot = return False -- tested all variables, unable to prove
    | otherwise =              
        mcase (proveAll ind_theory)     -- try induction on one variable
            (do
                modify (\s -> s{ind = Just num}) -- add variable used
                return True)                -- proves using induction on 'num'
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
prove :: Theory Id -> Induction Bool
prove th =
    let goal' = head $ passes th -- prepare conjecture using various passes
        goal''  = ppTheory' goal'
    in do
        --liftIO $ writeFile (out_path "goal.smt2") $ show $ goal''

        -- retrieve the preparation function from the Prover
        -- and apply it to the goal
        prob <- liftIO =<< (prepare <$> getProver) <*> (pure [show goal''])
        liftIO $ writeFile (out_path "prob.fof") $ prob

        -- run prover on the problem
        ep <- runProver $ out_path "prob.fof"

        -- check the output from the Prover by using
        -- the Provers parse function
        (b, ax) <- liftIO =<< (parseOut <$> getProver) <*> (pure [prob, ep])
        when (b) $ modify (\s -> s{axioms = (axioms s) ++ ax})
        return b








