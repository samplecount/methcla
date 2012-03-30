import Control.Concurrent (threadDelay)
import Control.Monad.IO.Class (liftIO)
import Control.Monad.Trans.Resource (runResourceT)
import Sound.Mescaline

main = do
    runResourceT $ runEngineT $ do
        start
        liftIO $ threadDelay (truncate 20e6)
    putStrLn "Freed engine"
    liftIO $ threadDelay (truncate 10e6)
