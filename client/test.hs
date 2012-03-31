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

prop_mapUriUnmap :: [Char] -> Property
prop_mapUriUnmap str = not (null str || elem '\NUL' str) ==> monadicIO (test (Uri.fromString str))
    where
        test uri = do
            uri' <- run $ runResourceT $ runEngineT $ Uri.map uri >>= Uri.unmap
            assert (Just uri == uri')

tests = [
    testProperty "URI map/unmap" prop_mapUriUnmap
    ]

main = defaultMain tests
-- main = quickCheck prop_mapUriUnmap
