
module Main where

import Control.Monad.State
import Data.List.Split
import Data.List
import Data.Maybe
import Data.Either
--import GHC.IO.Handle
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
import Tip.Pretty.Haskell as HS
import Tip.Lint
import Induction

import Prover hiding (getAxioms)
import Process


tip_file :: FilePath
tip_file = out_path "tip_file.smt2"

theory_file :: FilePath
theory_file = out_path "theory_file.txt"

prop_file :: FilePath
prop_file = out_path "prop.txt"


out_path :: FilePath -> FilePath
out_path = (++) "./out/"

main :: IO()
main = do
    
    preQS <- getInputFile

    -- created conjectures
    prop_string <- run_process "tip-spec" "." [preQS]
    writeFile prop_file prop_string

    -- parsing tip qith quickspec to theory
    theory_qs <- readTheory prop_file

    putStrLn ""
    putStrLn "---------- Now considering: ----------"
    -- Structural Induction
    case induct $ renameLemmas theory_qs of
        Ind a -> do 
                    ((th_done,_),(_,_,ls)) <- runStateT a (eprover, [],[])
                    printResult ls th_done 
        where -- count the number of conjectures in the theory
            numConj th = length $ fst $ theoryGoals th
            induct theory = loop_conj theory 0 (numConj theory) False

getInputFile :: IO FilePath
getInputFile = do
    -- query arguments for input file
    (path, file) <- (splitFileName . head) <$> getArgs
    case snd $ splitExtension file of
            ".smt2" -> return $ joinPath [path, file]
            ".hs"   -> do
                -- start a external Process for tip-ghc, translating a haskell file
                -- into smt2 (tip-format).
                tip_string <- run_process "tip-ghc" path [file]
                writeFile tip_file tip_string
                return tip_file
            _        -> fail "Incompatible file extension"


-- read a theory from a file with given filepath
readTheory :: FilePath -> IO (Theory Id)
readTheory fp = do
  theory_either <- parseFile fp
  -- check whether the parsing succeded
  case theory_either of
      Left x  -> fail $ "Failed to create theory: " ++ x
      Right theory -> return theory

-- The passes needed to convert the theory into tff format (+ skolemiseconjecture)
passes :: Theory Id -> [Theory Id]
passes = freshPass (runPasses
        [ SkolemiseConjecture
          , TypeSkolemConjecture
          , Monomorphise False
          , LambdaLift
          , AxiomatizeLambdas
          , SimplifyGently
          , CollapseEqual
          , RemoveAliases
          , SimplifyGently
          , Monomorphise False
          , IfToBoolOp
          , CommuteMatch
          , SimplifyGently
          , LetLift
          , SimplifyGently
          , AxiomatizeFuncdefs2
          , SimplifyGently
          , AxiomatizeDatadecls
        ])

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
            liftIO $ putStrLn $ "|       | " ++ formulaPrint 
            modify (\(p, _, ys) -> (p, [], ys))
            mcase (prove th) -- Test if solvable without induction
                (do -- Proved without induction
                    addLemma f_name False
                    loop_conj (provedConjecture curr theory) curr (num-1) continue)
                (mcase (loop_ind th 0 nbrVar) -- Attempt induction
                        (do -- Proved using induction
                            addLemma f_name True
                            loop_conj (provedConjecture curr theory) curr (num-1) True)
                        (loop_conj theory (curr + 1) num continue)) -- Unable to prove with current theory 

-- Stringify the body of a Formula
showFormula :: (Ord a, PrettyVar a) => Formula a -> String
showFormula fa =  concatMap repl splitStr
    where
        str = show $ ppExpr 0 $ fm_body fa
        splitStr = last (splitOn "\n" str)
        repl '\\' = []
        repl '"' = []
        repl c = [c]


-- Trying to prove a conjecture, looping over all variables in the conjecture
loop_ind :: Theory Id -> Int -> Int -> Induction Bool
loop_ind theory num tot 
    | num >= tot = return False -- tested all variables, unable to prove
    | otherwise =              
        mcase (proveAll ind_theory)     -- try induction on one variable
            (return True)               -- proves using induction on 'num'
            (loop_ind theory (num+1) tot)   -- unable to prove, try next variable
    where -- prepare theory for induction on variable 'num'
        ind_theory = freshPass (induction [num]) theory

-- Returns true if all conjectures are provable
proveAll :: [Theory Id] -> Induction Bool
proveAll []       = return True
proveAll (th:ths) = mcase (prove th)
                        (proveAll ths)
                        (return False)

-- case of for monadic bool
mcase :: (Monad m) => m Bool -> m a -> m a -> m a
mcase mbool t f = do
    bool <- mbool
    if bool then t else f

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
        when (b) $ modify (\(p, xs, ys) -> (p, xs ++ ax, ys))
        return b



-- run the choosen prover on the file given by the filepath
runProver :: FilePath -> Induction String
runProver source = liftIO =<< run_process <$> (name <$> getProver) <*> (pure ".") <*> fs
    where fs = (++ [source]) <$> (flags <$> getProver) :: Induction [Flag]


printResult :: [Lemma] -> Theory Id -> IO ()
printResult ls th = 
    do 
        let (notInd, ind) = partition (not . snd . snd) $ reverse ls
        putStrLn "\n-------------------------------------------------"
        putStrLn "Summary:"
        putStrLn ""
        putStrLn "Proved without induction"
        mapM_ putLemma notInd
        putStrLn ""
        putStrLn "Proved with induction"
        mapM_ putLemma ind
    where putLemma f@(name, (aux, _)) = do
            let thy_f = thy_asserts th
            let formula = lookupFormula name thy_f
            let str = subRegex (mkRegex "Tip\\.") (getFormula $ formula) ""
            let splitStr = dropWhile ((/=) '=') str
            let up = getUserProperty formula
            putStrLn $ (if isNothing up then name else fromJust up ) ++  " " ++ splitStr
            case aux of
                [] -> return ()
                ax -> mapM_ (\a -> putStrLn $ " | " ++ a) $ nub $ filter ((/=) name) ax


lookupFormula :: String -> [Formula Id] -> Maybe (Formula Id)
lookupFormula s [] = Nothing
lookupFormula s (f:fs) 
    | join (lookup "name" (fm_attrs f)) == Just s = Just f
    | otherwise = lookupFormula s fs

getFormula :: Maybe (Formula Id) -> String
getFormula Nothing = "Formula not found"
getFormula (Just f) = show $ pp . trTheory HS.Plain $ Theory [] [] [] [] [f] 

getUserProperty :: Maybe (Formula Id) -> Maybe String
getUserProperty Nothing = Nothing
getUserProperty (Just f) = join $ lookup "source" (fm_attrs f) 

getAssertion :: Formula Id -> Maybe String
getAssertion f = join $ lookup "name" (fm_attrs f)



