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

module Sound.LV2.Atom.Tuple (
    Tuple
  , fromList
  , toList
) where

import qualified Data.Foldable as F
import qualified Data.Traversable as T
import           Sound.LV2.Uri

newtype Tuple a = Tuple { toList :: [a] }
                    deriving (Functor, F.Foldable, Show, T.Traversable)

fromList :: [a] -> Tuple a
fromList = Tuple
