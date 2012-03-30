import Control.Concurrent (threadDelay)
import Control.Monad.IO.Class (liftIO)
import Control.Monad.Trans.Resource (runResourceT)
import Sound.LV2.Atom.Test
import Sound.Mescaline

main = do
    runResourceT $ runEngineT $ do
        start
        request_ fooBlah
        liftIO $ threadDelay (truncate 2e6)
    putStrLn "Freed engine"
    liftIO $ threadDelay (truncate 1e6)
