module Streamero.Readline (
    sourceReadline
) where

import           Control.Monad.IO.Class (MonadIO(..))
import qualified Data.Conduit as C
import qualified System.Console.Readline as R

sourceReadline :: MonadIO m => String -> C.Source m String
sourceReadline prompt = C.sourceState () $ \() -> do
    maybeLine <- liftIO $ R.readline prompt
    case maybeLine of
        Nothing     -> return C.StateClosed
        Just "exit" -> return C.StateClosed
        Just line   -> do liftIO $ R.addHistory line
                          return $ C.StateOpen () line
