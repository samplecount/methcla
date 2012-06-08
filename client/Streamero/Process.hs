module Streamero.Process (
    OutputHandler
  , proc
  , CreateProcess
  , ProcessHandle
  , createProcess
  , interruptProcess
  , withProcess
) where

import           Control.Monad (unless, void)
import           Control.Concurrent (forkIO, newEmptyMVar, putMVar, takeMVar)
import           Control.Exception (finally, onException, throw)
import           Streamero.Signals (ignoreSignals)
import           System.Exit (ExitCode(..))
import           System.IO (Handle)
import qualified System.IO as IO
import           System.Process (CreateProcess, ProcessHandle, terminateProcess, waitForProcess)
import qualified System.Process as P
import           System.Posix.Signals (keyboardSignal)

type OutputHandler = String -> IO ()

pipeOutput :: OutputHandler -> Handle -> IO ()
pipeOutput f h = IO.hIsEOF h >>= flip unless (IO.hGetLine h >>= f >> pipeOutput f h)

proc :: FilePath -> [String] -> CreateProcess
proc cmd args = (P.proc cmd args) { P.std_out = P.CreatePipe
                                  , P.std_err = P.CreatePipe }

createProcess :: OutputHandler -> CreateProcess -> IO ProcessHandle
createProcess handleOutput spec = do
    (_, maybeStdout, maybeStderr, pid) <- ignoreSignals [keyboardSignal] $ P.createProcess spec
    maybe (return ()) (void . forkIO . pipeOutput handleOutput) maybeStdout
    maybe (return ()) (void . forkIO . pipeOutput handleOutput) maybeStderr
    return pid

interruptProcess :: ProcessHandle -> IO ()
interruptProcess pid = do
    P.terminateProcess pid
    e <- P.waitForProcess pid
    case e of
        ExitSuccess -> return ()
        ExitFailure 15 -> return ()
        ExitFailure _  -> throw e

withProcess :: OutputHandler -> CreateProcess -> IO a -> IO a
withProcess handleOutput spec action = do
    pid <- createProcess handleOutput spec
    {-exitCode <- newEmptyMVar-}
    {-void $ forkIO $ waitForProcess pid >>= putMVar exitCode-}
    a <- action `finally` P.terminateProcess pid
    {-e <- takeMVar exitCode-}
    e <- P.waitForProcess pid
    case e of
        ExitSuccess -> return a
        ExitFailure 15 -> return a
        ExitFailure _ -> throw e
    return a

