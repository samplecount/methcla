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

import           Development.Shake
import           Development.Shake.FilePath
import qualified Methcla as Methcla
import qualified System.Directory as Dir

main :: IO ()
main = do
  isPro <- Dir.doesFileExist "../LICENSE-PRO"
  let variant = if isPro then Methcla.Pro else Methcla.Default
      sourceDir = "."
      buildDir = "build"
      shakeOptions' = shakeOptions {
                      shakeFiles = addTrailingPathSeparator buildDir
                    , shakeVersion = Methcla.version variant }
      f xs ts = do
          let options = foldl (.) id xs $ Methcla.defaultOptions
              rules = Methcla.mkRules variant sourceDir buildDir options Nothing
          return $ Just $ rules >> want ts
  shakeArgsWith shakeOptions' Methcla.optionDescrs f
