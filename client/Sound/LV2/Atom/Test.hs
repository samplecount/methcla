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

{-# LANGUAGE FlexibleInstances, MultiParamTypeClasses, OverloadedStrings, ScopedTypeVariables #-}
module Sound.LV2.Atom.Test where

import           Control.Applicative
import           Control.Monad.IO.Class (liftIO)
import           Control.Monad.Trans.Resource (MonadThrow)
import qualified Data.ByteString.Char8 as B
import           Data.Int
import           Data.Text (Text)
import qualified Data.Text as Text
import qualified Data.Vector.Storable as V
import           Data.Word
import           Sound.LV2.Atom.Get as Get
import           Sound.LV2.Atom.Put as Put
import           Sound.LV2.Atom.Object (Object, ObjectUri(..))
import qualified Sound.LV2.Atom.Object as Object
import qualified Sound.LV2.Uri as Uri

encodeShow g = do
    b <- encode g
    liftIO $ print (B.length b, b)
    return b

testFloat :: IO Float
testFloat = Uri.evalPureMap $ encodeShow (toAtom (pi::Float)) >>= decode fromAtom

testFloatFail :: IO Double
testFloatFail = Uri.evalPureMap $ encodeShow (toAtom (pi::Float)) >>= decode fromAtom

testBool :: IO (Bool, Bool)
testBool = Uri.evalPureMap $ encodeShow (toAtom (True, False)) >>= decode fromAtom

testStringTuple :: IO ()
testStringTuple = do
    x <- Uri.evalPureMap $ do
        b <- encodeShow (toAtom ("fuck yeah"::Text, [12::Int32]))
        flip decode b $ do
            (a1::Text, a2::[Int32]) <- fromAtom
            return (a1, a2)
    print x

testObject :: IO ()
testObject = do
    x <- Uri.evalPureMap $ do
        b <- encodeShow (toAtom (Object.fromList Blank (Uri.fromWord32 1) (Uri.fromWord32 2) [ ((Uri.fromWord32 0, Uri.fromWord32 4), [pi::Float])
                                                           , ((Uri.fromWord32 0, Uri.fromWord32 5), [1..12])]))
        flip decode b $ do
            (a1::Object [Float]) <- fromAtom
            return a1
    print x

testObjectTuple :: IO (Object Float, (Int32, Double))
testObjectTuple = do
    Uri.evalPureMap $ do
        encodeShow (toAtom (Object.fromList Blank (Uri.fromWord32 1) (Uri.fromWord32 2) [ ((Uri.fromWord32 0, Uri.fromWord32 4), pi::Float)
                                                      , ((Uri.fromWord32 0, Uri.fromWord32 5), 1) ]
                           , (42::Int32, pi::Double)))
            >>= decode fromAtom

data FooBlah = FooBlah {
    fooBlahA :: Word32
  , fooBlahB :: Text
  , fooBlahC :: Double
  , fooBlahD :: (Int32, Int32)
  } deriving (Show)

uri_fooBlahA = Uri.fromText "fooBlahA"
uri_fooBlahB = Uri.fromText "fooBlahB"
uri_fooBlahC = Uri.fromText "fooBlahC"
uri_fooBlahD = Uri.fromText "fooBlahD"

fooBlah = FooBlah 127 "Hell yeah" 1.1234 (1000, 2000)

instance Uri.Map m => ToAtom m FooBlah where
    toAtom f = Put.resource (Uri.fromText "Yo") (Uri.fromText "Blah") $ do
        property_ uri_fooBlahA (fooBlahA f)
        property_ uri_fooBlahB (fooBlahB f)
        property_ uri_fooBlahC (fooBlahC f)
        property_ uri_fooBlahD (fooBlahD f)

instance (Applicative m, MonadThrow m, Uri.Map m) => FromObject m FooBlah where
    fromObject o = FooBlah <$> o .: uri_fooBlahA
                           <*> o .: uri_fooBlahB
                           <*> o .: uri_fooBlahC
                           <*> o .: uri_fooBlahD

instance (Applicative m, MonadThrow m, Uri.Map m) => FromAtom m FooBlah where
    fromAtom = Get.object

testFooBlah :: IO FooBlah
testFooBlah = Uri.evalPureMap $
                encodeShow (toAtom fooBlah) >>= decode fromAtom
