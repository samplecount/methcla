module Streamero.Signals (
    ignoreSignals
  , catchSignals
) where

import Control.Exception (finally)
import Control.Monad (zipWithM_)
import System.Posix.Signals

ignoreSignals :: [Signal] -> IO a -> IO a
ignoreSignals signals action = do
    oldHandlers <- mapM (\sig -> installHandler sig Ignore Nothing) signals
    action `finally` zipWithM_ (\sig oldHandler -> installHandler sig oldHandler Nothing)
                               signals
                               oldHandlers

catchSignals :: IO () -> [Signal] -> IO ()
catchSignals action = mapM_ (\sig -> installHandler sig (Catch $ action) Nothing)
