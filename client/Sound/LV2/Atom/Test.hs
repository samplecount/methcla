{-# LANGUAGE OverloadedStrings, ScopedTypeVariables #-}
module Sound.LV2.Atom.Test where

import           Control.Monad.IO.Class (liftIO)
import qualified Data.ByteString.Char8 as B
import           Data.Int
import           Data.Text (Text)
import qualified Data.Text as Text
import qualified Data.Vector.Storable as V
import           Sound.LV2.Atom.Get
import           Sound.LV2.Atom.Put
import           Sound.LV2.Atom.Object (Object, ObjectUri(..))
import qualified Sound.LV2.Atom.Object as Object
import qualified Sound.LV2.Uri as Uri

testStringTuple :: IO ()
testStringTuple = do
    x <- Uri.evalPureMap $ do
        b <- encode (toAtom (Text.pack "fuck", [12::Int32]))
        liftIO $ print (B.unpack b)
        flip decode' b $ do
            (a1::Text, a2::[Int32]) <- fromAtom
            return (a1, a2)
    print x

testObject :: IO ()
testObject = do
    x <- Uri.evalPureMap $ do
        b <- encode (toAtom (Object.fromList Blank 1 2 [((0,4), [pi::Float]),((0,5),[1..12])]))
        liftIO $ print (B.unpack b)
        flip decode' b $ do
            (a1::Object [Float]) <- fromAtom
            return a1
    print x

test1 :: IO ()
test1 = do
    x <- Uri.evalPureMap $ do
        b <- encode (toAtom (Object.fromList Blank 1 2 [((0,4), pi::Float),((0,5),1)], 1::Int32))
        liftIO $ print (B.unpack b)
        flip decode' b $ do
            (a1::Object Float, a2::Int32) <- fromAtom
            return (a1, a2)
    print x
