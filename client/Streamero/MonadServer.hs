module Streamero.MonadServer (
  Handle
, new
, execute_
, execute
, executeWith
, quit
, wait
) where

import           Control.Concurrent (forkIO)
import qualified Control.Concurrent.Chan as Chan
import qualified Control.Concurrent.MVar as MVar
import qualified Control.Exception as E
import           Control.Monad.IO.Class (MonadIO, liftIO)

data Command m =
    Quit
  | Execute (m ())

-- | Monad server handle.
data Handle m = Handle (Command m -> IO ()) (MVar.MVar (Maybe E.SomeException))

-- | Create a new monad server handle.
new :: MonadIO m => (m () -> IO ()) -> IO (Handle m)
new f = do
  commands <- Chan.newChan
  result <- MVar.newEmptyMVar
  let loop = do
        cmd <- liftIO $ Chan.readChan commands
        case cmd of
            Quit -> return ()
            Execute action -> do
                action
                loop
  _ <- forkIO $ E.catch (f loop >> MVar.putMVar result Nothing)
                        (MVar.putMVar result . Just)
  return $ Handle (Chan.writeChan commands) result

-- | Execute action without waiting for result.
execute_ :: Handle m -> m () -> IO ()
execute_ (Handle sink _) = sink . Execute

-- | Execute action and receive result asynchronously in callback.
executeWith :: MonadIO m => Handle m -> (a -> IO ()) -> m a -> IO ()
executeWith handle callback action = execute_ handle (action >>= liftIO . callback)

-- | Execute action and wait for result. 
execute :: MonadIO m => Handle m -> m a -> IO a
execute handle action = do
  result <- MVar.newEmptyMVar
  executeWith handle (MVar.putMVar result) action
  MVar.takeMVar result

-- | Quit the server.
quit :: Handle m -> IO ()
quit (Handle sink _) = sink Quit

-- | Wait for the server to quit.
wait :: Handle m -> IO ()
wait (Handle _ result) = do
  x <- MVar.takeMVar result
  case x of
    Nothing -> return ()
    Just e  -> E.throw e

