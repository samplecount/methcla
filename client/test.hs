-- Copyright 2012-2013 Samplecount S.L.
-- 
-- Licensed under the Apache License, Version 2.0 (the "License");
-- you may not use this file except in compliance with the License.
-- You may obtain a copy of the License at
-- 
--     http://www.apache.org/licenses/LICENSE-2.0
-- 
-- Unless required by applicable law or agreed to in writing, software
-- distributed under the License is distributed on an "AS IS" BASIS,
-- WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
-- See the License for the specific language governing permissions and
-- limitations under the License.

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
