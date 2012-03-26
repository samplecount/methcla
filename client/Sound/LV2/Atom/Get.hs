{-# LANGUAGE FlexibleContexts
           , FlexibleInstances
           , MultiParamTypeClasses
           , ScopedTypeVariables #-}
module Sound.LV2.Atom.Get (
    Get
  , FromAtom(..)
  , GetObject
  , (.:), (.:?)
  , FromObject(..)
  , object, objectOf
  , decode
) where

import           Control.Applicative
import           Control.Monad
import           Control.Monad.Trans.Class (lift)
import           Control.Monad.Trans.Resource (MonadThrow(..))
import qualified Data.HashMap.Strict as H
import           Data.Text (Text)
import qualified Data.Text as Text
import qualified Data.Traversable as T
import           Data.Typeable (Typeable)
import qualified Data.ByteString as B
import qualified Data.ByteString.Internal as B
import qualified Data.ByteString.Lazy as BL
import           Data.Int (Int32, Int64)
import qualified Data.Vector.Storable as V
import           Data.Word
import           Sound.LV2.Atom.Class
-- import           Sound.LV2.Atom.Object (Object)
import qualified Sound.LV2.Atom.Object as Object
import           Sound.LV2.Atom.Parser (ParseException(..))
import qualified Sound.LV2.Atom.Parser as P
import           Sound.LV2.Atom.Primitive
import           Sound.LV2.Atom.Util
import           Sound.LV2.Uri (Uri, Urid)
import qualified Sound.LV2.Uri as Uri
import qualified Sound.LV2.Uri.Cache as Cache
import           Foreign.ForeignPtr (castForeignPtr)
import           Foreign.Storable (Storable)

import Debug.Trace

type Get m = P.Parser m

class FromAtom m a where
    fromAtom :: Get m a

word32 :: MonadThrow m => Get m Word32
word32 = P.takeStorable

urid :: MonadThrow m => Get m Urid
urid = liftM Uri.fromWord32 word32

atomBytes :: MonadThrow m => Get m B.ByteString
atomBytes = do
    size <- P.peekStorable
    P.take $ pad (headerSize + size)

header :: MonadThrow m => Get m (Urid, Word32)
header = do
    sz <- word32
    ui <- urid
    return $! (ui, sz)

-- | Align to the next alignment boundary.
align :: MonadThrow m => Get m ()
align = P.align pad

-- | Execute the parser and align to the next alignment boundary.
aligned :: MonadThrow m => Get m a -> Get m a
aligned g = do
    a <- g
    align
    return a

parseError :: MonadThrow m => String -> Get m a
parseError = monadThrow . ParseError

next :: (MonadThrow m, Uri.Map m) => (Urid -> Bool) -> (Urid -> Word32 -> Get m a) -> Get m a
next uriP f = do
    (ui, sz) <- header
    unless (uriP ui) $
        parseError $ "Invalid URID " ++ show ui
    aligned $ f ui sz

primitive :: forall m a . (MonadThrow m, Uri.Map m, Primitive a) => Get m a
primitive = do
    let uri = atomUri (undefined::a)
    ui <- Uri.map uri
    next (==ui) $ \_ sz' -> do
        let sz = fromIntegral (sizeOf (undefined :: a))
        when (sz' /= sz) $
            parseError
                $ "Invalid size " ++ show sz' ++ ", expected " ++ show sz ++ " (" ++ show uri ++ ")"
        toParser

instance (MonadThrow m, Uri.Map m) => FromAtom m Int32 where
    fromAtom = primitive
instance (MonadThrow m, Uri.Map m) => FromAtom m Int64 where
    fromAtom = primitive
instance (MonadThrow m, Uri.Map m) => FromAtom m Word32 where
    fromAtom = primitive
instance (MonadThrow m, Uri.Map m) => FromAtom m Word64 where
    fromAtom = primitive
instance (MonadThrow m, Uri.Map m) => FromAtom m Bool where
    fromAtom = primitive
instance (MonadThrow m, Uri.Map m) => FromAtom m Float where
    fromAtom = primitive
instance (MonadThrow m, Uri.Map m) => FromAtom m Double where
    fromAtom = primitive

stringOf :: (MonadThrow m, Uri.Map m) => Uri -> Get m Text
stringOf uri = do
    ui <- Uri.map uri
    next (==ui) $ \_ sz -> do
        t <- P.takeUtf8 (sz - 1)
        P.drop 1
        align
        return t

instance (MonadThrow m, Uri.Map m) => FromAtom m Text where
    fromAtom = stringOf Uri.string

tupleOf :: (MonadThrow m, Uri.Map m) => Get m a -> Get m a
tupleOf p = do
    ui <- Uri.map Uri.tuple
    next (==ui) $ \_ n -> P.isolate n p

instance (MonadThrow m, Uri.Map m, FromAtom m a1, FromAtom m a2) => FromAtom m (a1, a2) where
    fromAtom = tupleOf $ liftM2 (,) fromAtom fromAtom

vectorOf :: forall m a . (MonadThrow m, Uri.Map m, Atom a, Primitive a, Storable a) => a -> Get m (V.Vector a)
vectorOf _ = do
    ui <- Uri.map Uri.vector
    next (==ui) $ \_ sz -> do
        elSize' <- word32
        elType' <- urid

        elType <- Uri.map (atomUri (undefined :: a))
        when (elType' /= elType) $
            parseError
                $ "Invalid vector element type " ++ show elType' ++ ", expected " ++ show elType -- ++ " (" ++ show uri ++ ")"

        let elSize = sizeOf (undefined :: a)
        when (elSize' /= elSize) $
            parseError
                $ "Invalid vector element size " ++ show elSize' ++ ", expected " ++ show elSize

        b <- aligned $ P.take (sz - 2*sizeOf (undefined::Word32))

        let (fp, bo, _) = B.toForeignPtr b
            n = (sz - headerSize) `div` elSize
            (o, r) = fromIntegral bo `divMod` elSize
        if r == 0
            then if o == 0
                 then return $! V.unsafeFromForeignPtr0 (castForeignPtr fp) (fromIntegral n)
                 else return $! V.unsafeFromForeignPtr (castForeignPtr fp) (fromIntegral o) (fromIntegral n)
            else parseError $ "Whoops, unaligned vector offset, call your doctor"

instance (MonadThrow m, Uri.Map m, Atom a, Primitive a, Storable a) => FromAtom m (V.Vector a) where
    fromAtom = vectorOf (undefined::a)
instance (MonadThrow m, Uri.Map m, Atom a, Primitive a, Storable a) => FromAtom m [a] where
    fromAtom = liftM V.toList fromAtom

newtype GetObject = GetObject { toObject :: Object.Object B.ByteString }

(.:?) :: (MonadThrow m, Uri.Map m, Uri.ToUrid (Get m) u, FromAtom m a) => GetObject -> u -> Get m (Maybe a)
obj .:? key = do
    u <- Uri.toUrid key
    case Object.lookup (Uri.fromWord32 0, u) (toObject obj) of
        Nothing -> return Nothing
        Just b -> liftM Just $ lift $ decode fromAtom b

(.:) :: (MonadThrow m, Uri.Map m, Uri.ToUrid (Get m) u, Show u, FromAtom m a) => GetObject -> u -> Get m a
obj .: key = do
    a <- obj .:? key
    case a of
        Nothing -> parseError $ "Key not found: " ++ show key
        Just a -> return a

class FromObject m a where
    fromObject :: GetObject -> Get m a

-- instance Monad m => FromObject m (Object B.ByteString) where
--     fromObject = return

propertyBody :: MonadThrow m => Get m (Object.Key, B.ByteString)
propertyBody = do
    c <- urid
    k <- urid
    b <- atomBytes
    return $! ((c, k), b)

getObject :: (MonadThrow m, Uri.Map m) => Get m GetObject
getObject = do
    resourceId <- Uri.map Uri.resource
    blankId <- Uri.map Uri.blank
    next (\x -> x == resourceId || x == blankId) $ \ui sz -> do
        oid <- urid
        otype <- urid
        let n = pad sz - 2 * sizeOf (undefined :: Word32)
        ps <- P.isolate n (props [])
        return $! GetObject (Object.fromList
                             (if ui == resourceId then Object.Resource else Object.Blank)
                             oid otype ps)
    where
        props ps = do
            b <- P.atEnd
            if b then return ps
                 else do
                     p <- propertyBody
                     props $! p:ps

object :: (MonadThrow m, Uri.Map m, FromObject m a) => Get m a
object = getObject >>= fromObject

-- instance (MonadThrow m, Uri.Map m, FromObject m a) => FromAtom m a where
--     fromAtom = object

instance (MonadThrow m, Uri.Map m, FromAtom m a) => FromObject m (Object.Object a) where
    fromObject = T.mapM (lift . decode fromAtom) . toObject

objectOf :: (MonadThrow m, Uri.Map m, FromAtom m a) => a -> Get m (Object.Object a)
objectOf _ = getObject >>= fromObject

instance (MonadThrow m, Uri.Map m, FromAtom m a) => FromAtom m (Object.Object a) where
    fromAtom = objectOf (undefined :: a)

decode :: MonadThrow m => Get m a -> B.ByteString -> m a
decode g b = do
    (a, b') <- P.parse g b
    unless (B.null b')
        $ monadThrow $ ParseError $ "decode: left-over " ++ show (B.length b')
    return a

-- -- Uri.evalPureMap $ (encode (toAtom [(pi::Double), 1, 2, 3])) >>= Sound.LV2.Atom.Get.decode (Sound.LV2.Atom.Get.vectorDouble)
-- -- Uri.evalPureMap $ (encode (toAtom (Sound.LV2.Atom.Object.fromList Sound.LV2.Atom.Object.Blank 1 2 [((0,4), [pi::Float]),((0,5),[1.2])]))) >>= Sound.LV2.Atom.Get.decode' (Sound.LV2.Atom.Get.objectOf (undefined :: Float))
