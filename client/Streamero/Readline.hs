module Streamero.Readline (
    sourceReadline
) where

import           Control.Monad (unless)
import           Control.Monad.IO.Class (MonadIO(..))
import qualified Data.Conduit as C
import qualified System.Console.Editline.Readline as R

sourceReadline :: MonadIO m => String -> C.Source m String
sourceReadline prompt = C.sourceState () $ \() -> do
    maybeLine <- liftIO $ R.readline prompt
    case maybeLine of
        Nothing     -> return C.StateClosed
        Just "exit" -> return C.StateClosed
        Just line   -> do unless (null line) $ liftIO $ R.addHistory line
                          return $ C.StateOpen () line
