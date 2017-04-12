{-# LANGUAGE RecordWildCards #-}
module Induction.Proof where

import Data.List (find)
import Data.Maybe (fromMaybe)

import Control.Exception (Exception, SomeException, catch, throw)
import Control.Monad.State (modify, put, get, when, liftIO, join, runStateT)

import Induction.Induction (printStr, runProver, addLemma, nextTimeout)
import Induction.Types (TP(..), IndState(..), Lemma(..), Induction(..), IndPass, getLemmas, getProver, getInduction, getLoopNbr)

import Parser.Params (Params(..))

import Tip.Core (theoryGoals)
import Tip.Fresh (Name)
import Tip.Formula (getFormula)
import Tip.Passes (StandardPass(..),provedConjecture,selectConjecture, runPasses, freshPass)
import Tip.Types (Theory(..), Formula(..))

import Benchmarks (writeInterrupt)
import Constants (out_path)
import IO.Output (printTheories, removeContentInFolder)
import Prover (Prover(..))
import Utils (mcase, subsets)

-- Looping through all conjectures and try to prove them
-- If provable with structural induction, they are put into the theory as proven lemmas
-- If during one loop none is proved, do not continue
loop_conj :: Name a => Theory a -> TP a (Theory a)
loop_conj theory =  do
    let count = length . fst $ theoryGoals theory
    loopNbr <- getLoopNbr
    --liftIO $ putStrLn (show loopNbr)
    --liftIO $ putStrLn (show count)
    if ((loopNbr <= count) && (count /= 0)) then do
        state  <- get
        params <- params <$> get 
        lemmas <- getLemmas
        --select <- selectConj <$> getInduction
        proved <- provedConj <$> getInduction

        -- test a conjecture within a catch statement to enable writting results
        -- even with progrm failure       
        (theory', state) <- liftIO $ catch (runStateT (runTP $ do -- test next conjecture 
            
            -- pick a conjecture
--            ths <- select theory

            mcase (loop_conj2 theory)
                (do
                    --fail "proved"
                    pTheory <- proved theory
                    let count' = length . fst $ theoryGoals pTheory                    
                    if (count' < count) then do
                        --liftIO $ putStrLn "resetting!!"
                        resetLoop
                        else do
                            --liftIO $ putStrLn "incloop"
                            incLoop
            
                    loop_conj pTheory)
                (do
                    incLoop
                    let (gs, as) = theoryGoals theory
                    loop_conj theory{thy_asserts = tail gs ++ head gs:as})

                ) state 
            ) (\e -> do 
                    -- check for user interrupt, write interrupted state
                    when (show (e :: SomeException) == "user interrupt") ( do
                        when (benchmarks params) $ writeInterrupt lemmas "error"
                        fail "^C pressed")
                    throw e)
        put state -- add processed state when no error was encountered
        return theory'
        else
            return theory
    where 
        runTP (TP a) = a
        incLoop      = modify (\s -> s{loopNbr = (loopNbr s) + 1})
        resetLoop    = modify (\s -> s{loopNbr = 0})

loop_conj2 :: Name a => Theory a -> TP a (Bool)
loop_conj2 th = 
   let
        -- the formula matching the current conjecture
        formula     = head . fst . theoryGoals $ th
        -- lookup up unique name of formual
        f_name      = fromMaybe "no name"  $ join $ lookup "name" (fm_attrs formula)
        -- check if formula have a source name
        f_source    = join $ lookup "source" (fm_attrs formula)
        -- print name
        f_s         = fromMaybe f_name f_source

        -- turn formula into printable form
        formulaPrint = getFormula formula
    in do
        params <- params <$> get 
        select <- selectConj <$> getInduction

        -- clean output directory
        liftIO $ removeContentInFolder (out_path "")

        nbrVar  <- inductionSize <$> getInduction
        indPasses <- inductionPass <$> getInduction

        -- clean temporary state
        modify (\s -> s{axioms = [], ind=Nothing})
        mcase (select th >>= prove) -- Test if solvable without induction
            (do -- Proved without induction
                printStr 3 $ "| " ++ f_s  ++  "  -- P | " ++ formulaPrint
                addLemma f_name formulaPrint f_source-- add formula to proved lemmas
                -- go to next conjecture
                return True)
            (do
                let indVars = subsets (nbrInduct params) (nbrVar th)
                printStr 4 $ unlines ["", "= Indices to induct on =", " " ++ show indVars,""]
                mcase ( loop_ind th indPasses indVars ) -- Attempt induction
                    (do -- Proved using induction
                        printStr 3 $ "| " ++ f_s  ++  "  -- I | " ++ formulaPrint
                        addLemma f_name formulaPrint f_source-- add formula to proved lemmas
                        -- go to next conjecture
                        return True)
                    (do
                        -- Unable to prove with current theory
                        -- try next conjecture
                        printStr 3 $ "| " ++ f_s  ++  "  -- U | " ++ formulaPrint
                        return False))

-- Trying to prove a conjecture, looping over all variables in the conjecture
loop_ind :: Name a => Theory a -> [IndPass a] -> [[Int]] -> TP a Bool
loop_ind _      []     _  = return False -- tested all variables, unable to prove
loop_ind theory (f:fs) xs = do
        mcase (loop_var theory f xs)     -- try induction 
            (do -- proves using induction on x
                -- modify (\s -> s{ind = Just x}) -- store variables used
                return True)
            (loop_ind theory fs xs) -- unable to prove, try next variable


loop_var :: Name a => Theory a -> IndPass a -> [[Int]] -> TP a Bool
loop_var _ _ []                 = return False
loop_var theory indPass (x:xs) = do

        
        printStr 4 $ unwords [" Induction on indices", show x ]

        --prepare theory for induction on variables/application in x 
        -- indPasses <- inductionPass <$> getInduction

        printStr 3 "-----------------------------------------"

        let ind_theory = freshPass (indPass x) theory

        prep <- prepare <$> getProver
        liftIO $ printTheories prep ind_theory 0 (out_path ("Theory" ++ show x))

        mcase (proveAll ind_theory)     -- try induction 
            (do -- proves using induction on x
                modify (\s -> s{ind = Just x}) -- store variables used
                return True)
            (loop_var theory indPass xs) -- unable to prove, try next variable

-- Returns true if all conjectures are provable
proveAll :: Name a => [Theory a] -> TP a Bool
proveAll []       = return True
proveAll (th:ths) = mcase (prove th)
                        (proveAll ths)
                        (return False)


-- trying to prove one conjecture, returns true if provable
-- TODO passes should be prover dependent!!!
-- TODO Do something proper about multi-theories
prove :: Name a => Theory a -> TP a Bool
prove th = do

        -- retrieve the preparation function from the Prover
        -- and apply it to the goal
        prob <- liftIO =<< (prepare <$> getProver) <*> pure th
        liftIO $ writeFile (out_path "problem") prob

        -- run prover on the problem
        ep <- runProver $ out_path "problem"

        -- check the output from the Prover by using
        -- the Provers parse function
        (proved, ax) <- liftIO =<< (parseOut <$> getProver) <*> pure [prob, ep]
        
        -- get lemma names
        lemmas <- getLemmas
        ax' <- mapM (\ln -> 
                    case find (\Lemma{..} -> lemmaName == ln) lemmas of
                        Nothing -> return ln --fail "WTF!!!"
                        Just l  -> return ln {- $ fromMaybe ln (lemmaSource l)-}) ax

        -- add auxilliary lemmas to temporary state
        when proved $ modify (\s -> s{axioms = axioms s ++ ax'})
        return proved
