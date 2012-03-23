{-# LANGUAGE DeriveDataTypeable, FlexibleInstances, MultiParamTypeClasses, OverloadedStrings, ScopedTypeVariables #-}
module Sound.LV2.Atom.Get where

import Control.Applicative
-- import Data.Attoparsec as P
import           Control.Exception (Exception)
import           Control.Monad
import           Control.Monad.ST (runST)
import           Control.Monad.Trans.Resource (runExceptionT)
import           Control.Monad.Trans.Class (lift)
import qualified Data.Conduit as C
import qualified Data.Conduit.List as CL
import qualified Data.Conduit.Binary as CB
import qualified Data.Conduit.Text as CT
import qualified Data.HashMap.Strict as H
import           Data.Text (Text)
import qualified Data.Text as Text
import qualified Data.Traversable as T
import           Data.Typeable (Typeable)
-- import Data.Attoparsec.Binary
import qualified Data.ByteString as B
import qualified Data.ByteString.Internal as B
import qualified Data.ByteString.Lazy as BL
import           Data.Int (Int32, Int64)
import qualified Data.Vector.Storable as V
import           Data.Word
import           Sound.LV2.Atom.Class
import           Sound.LV2.Atom.Object (Object)
import qualified Sound.LV2.Atom.Object as Object
import           Sound.LV2.Atom.Primitive
import           Sound.LV2.Atom.Util
import           Sound.LV2.Uri (Uri, Urid)
import qualified Sound.LV2.Uri as Uri
import qualified Sound.LV2.Uri.Cache as Cache
import           Foreign.ForeignPtr (castForeignPtr)
import           Foreign.Storable (Storable)

import Debug.Trace

data ParseException = ParseException String
    deriving (Show, Typeable)

instance Exception ParseException

type Get m a = C.Sink B.ByteString m a

class FromAtom m a where
    fromAtom :: Get m a

-- | Get a number of bytes as a strict 'ByteString'.
-- FIXME: How to make the lazy to strict conversion more efficient?
bytes :: C.MonadThrow m => Word32 -> Get m B.ByteString
bytes n = do
    b <- liftM (B.concat . BL.toChunks) . CB.take . fromIntegral $ n
    unless (fromIntegral n == B.length b) $
        throw $ ParseException
              $ "Need " ++ show n ++ " bytes, got " ++ show (B.length b)
    return b

-- FIXME: How to solve this without ScopedTypeVariables?
primitive_ :: forall m a . (C.MonadThrow m, Primitive a) => Get m a
primitive_ = liftM fromByteString (bytes (sizeOf (undefined::a)))

word32 :: C.MonadThrow m => Get m Word32
word32 = primitive_

atomBytes :: Monad m => Get m BL.ByteString
atomBytes = do
    let n = sizeOf (undefined :: Word32)
    sizeBytes <- CB.take (fromIntegral n)
    let size = castByteString n (B.concat (BL.toChunks sizeBytes))
    bytes <- CB.take (fromIntegral (sizeOf (undefined :: Urid) + pad size))
    -- align (headerSize + size)
    return $! sizeBytes `BL.append` bytes

header :: C.MonadThrow m => Get m (Urid, Word32)
header = do
    size <- word32
    urid <- word32
    return $! (urid, size)

-- | Align to the next alignment boundary.
align :: Monad m => Word32 -> Get m ()
align n = CB.take (fromIntegral (padding n)) >> return ()

-- | Throw an exception.
throw :: C.MonadThrow m => Exception e => e -> Get m a
throw = lift . C.monadThrow

next :: (C.MonadThrow m, Uri.Map m) => (Urid -> Bool) -> (Urid -> Word32 -> Get m a) -> Get m a
next uriP f = do
    -- ui <- urid uri
    (ui, sz) <- header
    unless (uriP ui) $
        throw $ ParseException
              $ "Invalid URID " ++ show ui
    a <- CB.isolate (fromIntegral sz) C.=$ f ui sz
    align sz
    return a

urid :: Uri.Map m => Uri -> Get m Urid
urid = lift . Uri.map

primitive :: forall m a . (C.MonadThrow m, Uri.Map m, Primitive a) => Get m a
primitive = do
    ui <- urid (atomUri (undefined::a))
    next (==ui) $ \_ sz' -> do
        let sz = fromIntegral (sizeOf (undefined :: a))
        when (sz' /= sz) $
            throw $ ParseException
                  $ "Invalid size " ++ show sz' ++ ", expected " ++ show sz -- ++ " (" ++ show uri ++ ")"
        primitive_

instance (C.MonadThrow m, Uri.Map m) => FromAtom m Int32 where
    fromAtom = primitive
instance (C.MonadThrow m, Uri.Map m) => FromAtom m Int64 where
    fromAtom = primitive
instance (C.MonadThrow m, Uri.Map m) => FromAtom m Word32 where
    fromAtom = primitive
instance (C.MonadThrow m, Uri.Map m) => FromAtom m Word64 where
    fromAtom = primitive
instance (C.MonadThrow m, Uri.Map m) => FromAtom m Bool where
    fromAtom = primitive
instance (C.MonadThrow m, Uri.Map m) => FromAtom m Float where
    fromAtom = primitive
instance (C.MonadThrow m, Uri.Map m) => FromAtom m Double where
    fromAtom = primitive

stringOf :: (C.MonadThrow m, Uri.Map m) => Uri -> Get m Text
stringOf uri = do
    ui <- urid uri
    next (==ui) $ \_ sz -> do
        let n = sz - 1
        ts <- (CB.isolate (fromIntegral sz - 1) C.=$= CT.decode CT.utf8) C.=$ CL.consume
        CB.take 1
        align sz
        return $! Text.concat ts

instance (C.MonadThrow m, Uri.Map m) => FromAtom m Text where
    fromAtom = stringOf Uri.string

-- tuple :: (C.MonadThrow m, Uri.Map m) => Get m [Get m ()]
instance (C.MonadThrow m, Uri.Map m, FromAtom m a1, FromAtom m a2) => FromAtom m (a1, a2) where
    fromAtom = do
        ui <- urid Uri.tuple
        next (==ui) $ \_ _ -> do
            a1 <- fromAtom
            a2 <- fromAtom
            return (a1, a2)

vectorOf :: forall m a . (C.MonadThrow m, Uri.Map m, Atom a, Primitive a, Storable a) => a -> Get m (V.Vector a)
vectorOf _ = do
    ui <- urid Uri.vector
    next (==ui) $ \_ sz -> do
        elSize' <- word32
        elType' <- word32

        elType <- urid (atomUri (undefined :: a))
        when (elType' /= elType) $
            throw $ ParseException
                  $ "Invalid vector element type " ++ show elType' ++ ", expected " ++ show elType -- ++ " (" ++ show uri ++ ")"

        let elSize = sizeOf (undefined :: a)
        when (elSize' /= elSize) $
            throw $ ParseException
                  $ "Invalid vector element size " ++ show elSize' ++ ", expected " ++ show elSize

        b <- bytes (sz - 2*sizeOf (undefined::Word32))
        align sz
        let (fp, bo, _) = B.toForeignPtr b
            n = (sz - headerSize) `div` elSize
            (o, r) = fromIntegral bo `divMod` elSize
        if r == 0
            then if o == 0
                 then return $! V.unsafeFromForeignPtr0 (castForeignPtr fp) (fromIntegral n)
                 else return $! V.unsafeFromForeignPtr (castForeignPtr fp) (fromIntegral o) (fromIntegral n)
            else throw $ ParseException
                       $ "Whoops, unaligned vector offset, call your doctor"

instance (C.MonadThrow m, Uri.Map m, Atom a, Primitive a, Storable a) => FromAtom m (V.Vector a) where
    fromAtom = vectorOf (undefined::a)
instance (C.MonadThrow m, Uri.Map m, Atom a, Primitive a, Storable a) => FromAtom m [a] where
    fromAtom = liftM V.toList fromAtom

(.:?) :: (Uri.Map m, FromAtom m a) => Object BL.ByteString -> Object.Key -> Get m (Maybe a)
obj .:? key = do
    case Object.lookup key obj of
        Nothing -> return Nothing
        Just b -> liftM Just $ lift $ decode fromAtom b

class FromObject m a where
    fromObject :: Object BL.ByteString -> Get m a

instance Monad m => FromObject m (Object BL.ByteString) where
    fromObject = return

propertyBody :: C.MonadThrow m => Get m (Object.Key, BL.ByteString)
propertyBody = do
    c <- word32
    k <- word32
    b <- atomBytes
    return $! ((c, k), b)

object :: (C.MonadThrow m, Uri.Map m, FromObject m a) => Get m a
object = do
    resourceId <- urid Uri.resource
    blankId <- urid Uri.blank
    next (\x -> x == resourceId || x == blankId) $ \ui sz -> do
        oid <- word32
        otype <- word32
        let n = sz - 2 * sizeOf (undefined :: Word32)
        ps <- CB.isolate (fromIntegral n) C.=$ props []
        fromObject $! Object.fromList
                    (if ui == resourceId then Object.Resource else Object.Blank)
                    oid otype ps
    where
        props ps = do
            m <- CL.peek
            case m of
                Nothing ->
                    return $! ps
                Just _ -> do
                    p <- propertyBody
                    props $! p:ps

objectOf :: (C.MonadThrow m, Uri.Map m, FromAtom m a) => a -> Get m (Object a)
objectOf _ = object >>= T.mapM (lift . decode fromAtom)

instance (C.MonadThrow m, Uri.Map m, FromAtom m a) => FromAtom m (Object a) where
    fromAtom = objectOf (undefined :: a)

decode :: Monad m => Get m a -> BL.ByteString -> m a
decode a b = CL.sourceList (BL.toChunks b) C.$$ a

decode' :: Monad m => Get m a -> B.ByteString -> m a
decode' a b = CL.sourceList [b] C.$$ a

-- Uri.evalPureMap $ (encode (toAtom [(pi::Double), 1, 2, 3])) >>= Sound.LV2.Atom.Get.decode (Sound.LV2.Atom.Get.vectorDouble)
-- Uri.evalPureMap $ (encode (toAtom (Sound.LV2.Atom.Object.fromList Sound.LV2.Atom.Object.Blank 1 2 [((0,4), [pi::Float]),((0,5),[1.2])]))) >>= Sound.LV2.Atom.Get.decode' (Sound.LV2.Atom.Get.objectOf (undefined :: Float))
