{-# LANGUAGE TupleSections #-}
module IO.Process where

import           Jukebox.Options    (parser, runPar)
import           Jukebox.Toolbox    (toFofBox)
import           Jukebox.TPTP.Parse (parseString)
import           Jukebox.TPTP.Print (showProblem)
import           System.IO          (hGetContents)
import           System.Process     (StdStream (CreatePipe), CreateProcess(..), CmdSpec(..), createProcess, cwd,
                                     proc, std_out, waitForProcess, shell,
                                     readProcessWithExitCode)
import           Tip.Parser         (Id, parseFile)
import           Tip.Types          (Theory)


-- read a theory from a file with given filepath
readTheory :: FilePath -> IO (Theory Id)
readTheory fp = do
  theory_either <- parseFile fp
  -- check whether the parsing succeded
  case theory_either of
      Left x       -> fail $ "Failed to create theory: " ++ x
      Right theory -> return theory

-- run a process with name, working dir path, and a list of arguments
-- return any output generated by the command
run_process :: String -> FilePath  -> [String] -> IO String
run_process name path ops = 
    exec_process( proc name ops )
        { cwd = Just path, std_out = CreatePipe  }


-- run a process with name, working dir path, and a list of arguments
-- return any output generated by the command. The command will be "raw",
-- i.e. passes the the shell as is. 'run_process' ensures the 
-- all options will be passed on to the executable.
run_process' :: String -> FilePath -> [String] -> IO String
run_process' name path ops = 
    exec_process( shell (unwords $ name:ops) )
        { cwd = Just path, std_out = CreatePipe  }

-- run a process with name, working dir path, and a list of arguments
-- return any output generated by the command. The command will be "raw",
-- i.e. passes the the shell as is. 'run_process' ensures the 
-- all options will be passed on to the executable.
run_process1 :: String -> FilePath -> [String] -> IO String
run_process1 name path ops = do
      (ec,stdout,stderr) <- readProcessWithExitCode name ops ""
      return stdout 

exec_process :: CreateProcess -> IO String
exec_process cp = do
    (_,proc,_,p_id) <- createProcess cp
    waitForProcess p_id
    case proc of
        Nothing     -> fail $ "Error: Could not run '" ++ (case cmdspec cp of
                                                                ShellCommand s -> s
                                                                RawCommand s ls -> unwords (s:ls)) ++ "'"
        Just handle -> hGetContents handle

-- Calls Jukebox to turn a tff problem into fof
jukebox_hs :: String -> IO String
jukebox_hs problem = do
    -- Box to turn tff to fof
    let pars = parser toFofBox
    -- parse the problem
    problemForm <- parseString problem
    -- run jukebox quietly
    case runPar pars ["--quiet"] of
        Right mToFof    -> do
            toFof <- mToFof     -- extractin fof-function from monad
            showProblem <$> toFof problemForm   -- translate tff to fof

        -- Somethin unexpected occured
        -- TODO do something with error
        Left err        -> fail "Error: jukebox kunde inte köras"