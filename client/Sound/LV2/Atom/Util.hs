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

module Sound.LV2.Atom.Util (
    alignment
  , pad
  , padding
  , headerSize
) where

import Data.Bits
import Data.Word

{-# INLINE alignment #-}
alignment :: Word32
alignment = 8

{-# INLINE pad #-}
pad :: Word32 -> Word32
pad size = (size + mask) .&. (complement mask)
    where mask = alignment - 1

{-# INLINE padding #-}
padding :: Word32 -> Word32
padding size = pad size - size

{-# INLINE headerSize #-}
headerSize :: Word32
headerSize = 8
