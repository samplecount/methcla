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

{-# LANGUAGE RankNTypes #-}
module Sound.LV2.Atom.Primitive (
    Primitive(..)
) where

import           Blaze.ByteString.Builder (Builder)
import qualified Blaze.ByteString.Builder as B
import           Control.Exception (assert)
import           Control.Monad
import           Control.Monad.Trans.Resource (MonadThrow)
import           Data.ByteString (ByteString)
import           Data.Int (Int32, Int64)
import           Data.Word (Word32, Word64)
import           Sound.LV2.Atom.Class
import           Sound.LV2.Atom.Parser (Parser, ParseException)
import qualified Sound.LV2.Atom.Parser as P

class Atom a => Primitive a where
    sizeOf :: a -> Word32
    toBuilder :: a -> Builder
    toParser :: forall m . MonadThrow m => Parser m a

instance Primitive Int32 where
    sizeOf _ = 4
    toBuilder = B.fromStorable
    toParser = P.takeStorable

instance Primitive Word32 where
    sizeOf _ = 4
    toBuilder = B.fromStorable
    toParser = P.takeStorable

instance Primitive Int64 where
    sizeOf _ = 8
    toBuilder = B.fromStorable
    toParser = P.takeStorable

instance Primitive Word64 where
    sizeOf _ = 8
    toBuilder = B.fromStorable
    toParser = P.takeStorable

instance Primitive Bool where
    sizeOf _ = sizeOf (undefined :: Int32)
    toParser = liftM fromInt32 toParser
        where
            fromInt32 :: Int32 -> Bool
            fromInt32 0 = False
            fromInt32 _ = True
    toBuilder = toBuilder . toInt32
        where
            toInt32 :: Bool -> Int32
            toInt32 True = 1
            toInt32 False = 0

instance Primitive Float where
    sizeOf _ = 4
    toBuilder = B.fromStorable
    toParser = P.takeStorable

instance Primitive Double where
    sizeOf _ = 8
    toBuilder = B.fromStorable
    toParser = P.takeStorable
