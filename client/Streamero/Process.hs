module Streamero.Process (
    OutputHandler
  , proc
  , CreateProcess
  , ProcessHandle
  , createProcess
  , withProcess
) where

import           Control.Monad (unless, void)
import           Control.Concurrent (forkIO, newEmptyMVar, putMVar, takeMVar)
import           Control.Exception (finally, throw)
import           System.Exit (ExitCode(..))
import           System.IO (Handle)
import qualified System.IO as IO
import           System.Process (CreateProcess, ProcessHandle, terminateProcess, waitForProcess)
import qualified System.Process as P

type OutputHandler = String -> IO ()

pipeOutput :: OutputHandler -> Handle -> IO ()
pipeOutput f h = IO.hIsEOF h >>= flip unless (IO.hGetLine h >>= f >> pipeOutput f h)

proc :: FilePath -> [String] -> CreateProcess
proc cmd args = (P.proc cmd args) { P.std_out = P.CreatePipe
                                  , P.std_err = P.CreatePipe }

createProcess :: OutputHandler -> CreateProcess -> IO ProcessHandle
createProcess handleOutput spec = do
    (_, maybeStdout, maybeStderr, pid) <- P.createProcess spec
    maybe (return ()) (void . forkIO . pipeOutput handleOutput) maybeStdout
    maybe (return ()) (void . forkIO . pipeOutput handleOutput) maybeStderr
    return pid

withProcess :: OutputHandler -> CreateProcess -> IO a -> IO a
withProcess handleOutput spec action = do
    pid <- createProcess handleOutput spec
    exitCode <- newEmptyMVar
    void $ forkIO $ waitForProcess pid >>= putMVar exitCode
    a <- action `finally` terminateProcess pid
    e <- takeMVar exitCode
    case e of
        ExitSuccess -> return a
        ExitFailure _ -> throw e
