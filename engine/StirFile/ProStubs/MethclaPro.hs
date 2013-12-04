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

module MethclaPro (
    isPresent
  , engineSources
  , pluginSources
  , testSources
  , testBuildFlags
) where

import           Shakefile.C (BuildFlags, Target)
import           Shakefile.SourceTree (SourceTree)
import qualified Shakefile.SourceTree as SourceTree

isPresent :: Bool
isPresent = False

engineSources :: Target -> SourceTree BuildFlags
engineSources = const SourceTree.empty

pluginSources :: Target -> [(String, SourceTree BuildFlags)]
pluginSources = const []

testSources :: Target -> SourceTree BuildFlags
testSources = const SourceTree.empty

testBuildFlags :: Target -> BuildFlags -> BuildFlags
testBuildFlags = const id
