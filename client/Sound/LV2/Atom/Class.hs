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

{-# LANGUAGE MultiParamTypeClasses #-}
module Sound.LV2.Atom.Class (
    Atom(..)
) where

import Data.Int
import Data.Word
import Sound.LV2.Uri as Uri

class Atom a where
    atomUri :: a -> Uri

instance Atom Int32 where
    atomUri _ = Uri.int32

instance Atom Int64 where
    atomUri _ = Uri.int64

instance Atom Word32 where
    atomUri _ = atomUri (undefined::Int32)

instance Atom Word64 where
    atomUri _ = atomUri (undefined::Int64)

instance Atom Bool where
    atomUri _ = Uri.bool

instance Atom Float where
    atomUri _ = Uri.float

instance Atom Double where
    atomUri _ = Uri.double
