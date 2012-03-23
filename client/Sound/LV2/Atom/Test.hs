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

fooBlah = FooBlah 127 "Hell yeah" 1.1234 (1000, 2000)

instance Uri.Map m => ToAtom m FooBlah where
    toAtom f = Put.object Blank (Uri.fromText "Yo") (Uri.fromText "Blah") $ do
        property (Uri.fromWord32 0) (Uri.fromText "fooBlahA") (fooBlahA f)
        property (Uri.fromWord32 0) (Uri.fromText "fooBlahB") (fooBlahB f)
        property (Uri.fromWord32 0) (Uri.fromText "fooBlahC") (fooBlahC f)
        property (Uri.fromWord32 0) (Uri.fromText "fooBlahD") (fooBlahD f)
        -- toAtom $ Object.fromList Blank 10 20
        --     [ property 0 u_fooBlahA (fooBlahA f)
        --     , property 0 u_fooBlahB (fooBlahB f)
        --     , property 0 u_fooBlahC (fooBlahC f)
        --     , property 0 u_fooBlahD (fooBlahD f) 
        --     ]

instance (Applicative m, MonadThrow m, Uri.Map m) => FromObject m FooBlah where
    fromObject o = FooBlah <$> o .: (Uri.fromText "fooBlahA")
                           <*> o .: (Uri.fromText "fooBlahB")
                           <*> o .: (Uri.fromText "fooBlahC")
                           <*> o .: (Uri.fromText "fooBlahD")

instance (Applicative m, MonadThrow m, Uri.Map m) => FromAtom m FooBlah where
    fromAtom = getObject >>= fromObject

testFooBlah :: IO FooBlah
testFooBlah = Uri.evalPureMap $
                encodeShow (toAtom fooBlah) >>= decode fromAtom

