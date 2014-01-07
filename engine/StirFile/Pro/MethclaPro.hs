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
  , versionTags
  , engineSources
  , pluginSources
  , testSources
  , testBuildFlags
) where

import           Methcla.Util (under)
import           Shakefile.C (BuildFlags, Target, isTargetOS, linkerFlags, onlyIf)
import           Shakefile.Label (append)
import           Shakefile.SourceTree (SourceTree)
import qualified Shakefile.SourceTree as SourceTree

isPresent :: Bool
isPresent = True

versionTags :: [String]
versionTags = ["pro"]

isDarwin :: Target -> Bool
isDarwin = isTargetOS (Just "apple") (Just "darwin10")

engineSources :: FilePath -> Target -> SourceTree BuildFlags
engineSources sourceDir _ = SourceTree.files $ under sourceDir [
    "src/Methcla/ProAPI.cpp"
  ]

pluginSources :: FilePath -> Target -> [SourceTree BuildFlags]
pluginSources sourceDir target = [
    SourceTree.files $ under sourceDir [ "plugins/pro/disksampler.cpp" ]
  ]
  ++
  if isDarwin target
    then [ SourceTree.files $ under sourceDir [ "plugins/pro/soundfile_api_extaudiofile.cpp" ] ]
    else []

testSources :: FilePath -> Target -> SourceTree BuildFlags
testSources sourceDir _ = SourceTree.files $
  under sourceDir [ "tests/methcla_pro_engine_tests.cpp" ]

testBuildFlags :: Target -> BuildFlags -> BuildFlags
testBuildFlags target =
  onlyIf (isDarwin target) $
    append linkerFlags ["-framework", "AudioToolbox", "-framework", "CoreFoundation"]
