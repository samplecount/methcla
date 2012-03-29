import           Bindings.Sound.Mescaline
import           Control.Monad.IO.Class (liftIO)
import           Control.Monad.Trans.Class (lift)
import           Control.Monad.Trans.Resource (runResourceT)
import           Control.Concurrent
import           Control.Exception (bracket)
import qualified Data.Text as Text
import           Foreign.C

import qualified Sound.LV2.Uri as Uri
import           Sound.Mescaline
import           System.Environment

import Test.Framework (defaultMain, testGroup)
import Test.Framework.Providers.QuickCheck2 (testProperty)
import Test.QuickCheck (arbitrary, Property, quickCheck, (==>))
import Test.QuickCheck.Monadic (assert, monadicIO, pick, pre, run)

-- withEngine = bracket c'Mescaline_Engine_new c'Mescaline_Engine_free

prop_mapUriUnmap :: [Char] -> Property
prop_mapUriUnmap str = not (null str || elem '\NUL' str) ==> monadicIO (test (Uri.fromString str))
    where
        test uri = do
            uri' <- run $ runResourceT $ runEngineT $ Uri.map uri >>= Uri.unmap
            assert (Just uri == uri')

tests = [
    testProperty "URI map/unmap" prop_mapUriUnmap
    ]

-- main = do
--     args <- getArgs
--     case args of
--         ["test"] -> defaultMain tests
--         _        -> return ()

main = defaultMain tests
-- main = quickCheck prop_mapUriUnmap

main_ = do
    e <- c'Mescaline_Engine_new
    c'Mescaline_Engine_start e
    threadDelay (truncate 2e6)
    -- c'Mescaline_Engine_stop e
    putStrLn "Freeing engine"
    c'Mescaline_Engine_free e
    putStrLn "Freed engine"    
    threadDelay (truncate 20e6)
