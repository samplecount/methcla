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
import qualified Data.HashMap.Strict as H
import           Data.Typeable (Typeable)
-- import Data.Attoparsec.Binary
import qualified Data.ByteString as B
import qualified Data.ByteString.Internal as B
import qualified Data.ByteString.Lazy as BL
import           Data.Int (Int32, Int64)
import qualified Data.Vector.Storable as V
import           Data.Word
import           Sound.LV2.Atom.Primitive
import           Sound.LV2.Atom.Util
import           Sound.LV2.Uri (Uri, Urid)
import qualified Sound.LV2.Uri as Uri
import           Foreign.ForeignPtr (castForeignPtr)
import           Foreign.Storable (Storable)

data ParseException = ParseException String
    deriving (Show, Typeable)

instance Exception ParseException

type Get m a = C.Sink B.ByteString m a

class Uri.Map m => FromAtom m a where
    fromAtom :: Get m a

-- | Get a number of bytes as a strict 'ByteString'.
-- FIXME: How to make the lazy to strict conversion more efficient?
bytes :: Monad m => Word32 -> Get m B.ByteString
bytes = liftM (B.concat . BL.toChunks) . CB.take . fromIntegral

-- FIXME: How to solve this without ScopedTypeVariables?
primitive_ :: forall m a . (Monad m, Primitive a) => Get m a
primitive_ = liftM fromByteString (bytes (sizeOf (undefined::a)))

word32_ :: Monad m => Get m Word32
word32_ = primitive_

atomBytes :: Monad m => Get m BL.ByteString
atomBytes = do
    let n = sizeOf (undefined :: Word32)
    sizeBytes <- CB.take (fromIntegral n)
    let size = castByteString n (B.concat (BL.toChunks sizeBytes))
    bytes <- CB.take (fromIntegral (sizeOf (undefined :: Urid) + size))
    align (headerSize + size)
    return $! sizeBytes `BL.append` bytes

header :: Monad m => Get m (Urid, Word32)
header = do
    size <- word32_
    urid <- word32_
    return $! (urid, size)

align :: Monad m => Word32 -> Get m ()
align n = CB.take (fromIntegral (padding n)) >> return ()

throw :: C.MonadThrow m => Exception e => e -> Get m a
throw = lift . C.monadThrow

next :: (C.MonadThrow m, Uri.Map m) => (Urid -> Bool) -> (Word32 -> Get m a) -> Get m a
next uriP f = do
    -- ui <- urid uri
    (ui', sz) <- header
    unless (uriP ui') $
        throw $ ParseException
              $ "Invalid URID " ++ show ui'
    a <- CB.isolate (fromIntegral sz) C.=$ f sz
    align sz
    return a

-- primitiveBody :: forall m a . (C.MonadResource m, Primitive a) => Get m a
-- primitiveBody = do
--     a <- primitive_
--     align (sizeOf (undefined :: a))
--     return a

primitive :: forall m a . (C.MonadThrow m, Uri.Map m, Primitive a) => Uri -> Get m a
primitive uri = do
    ui <- urid uri
    next (==ui) $ \sz' -> do
        let sz = fromIntegral (sizeOf (undefined :: a))
        when (sz' /= sz) $
            throw $ ParseException
                  $ "Invalid size " ++ show sz' ++ ", expected " ++ show sz ++ " (" ++ uri ++ ")"
        primitive_

-- primitive uri = do
--     ui <- urid uri
--     let sz = fromIntegral (sizeOf (undefined :: a))
--     (ui', sz') <- header
--     when (ui' /= ui) $
--         lift $ C.resourceThrow (ParseException $ "Invalid URID " ++ show ui' ++ ", expected " ++ show ui ++ " (" ++ uri ++ ")")
--     when (sz' /= sz) $
--         lift $ C.resourceThrow (ParseException $ "Invalid size " ++ show sz' ++ ", expected " ++ show sz ++ " (" ++ uri ++ ")")
--     primitiveBody

urid :: Uri.Map m => Uri -> Get m Urid
urid = lift . Uri.map

int32 :: (C.MonadThrow m, Uri.Map m) => Get m Int32
int32 = primitive Uri.int32

int64 :: (C.MonadThrow m, Uri.Map m) => Get m Int64
int64 = primitive Uri.int64

bool :: (C.MonadThrow m, Uri.Map m) => Get m Bool
bool = primitive Uri.bool

float :: (C.MonadThrow m, Uri.Map m) => Get m Float
float = primitive Uri.float

double :: (C.MonadThrow m, Uri.Map m) => Get m Double
double = primitive Uri.double

-- tuple :: (C.MonadThrow m, Uri.Map m) => Get m [Get m ()]

vector :: forall m a . (C.MonadThrow m, Uri.Map m, Storable a, Primitive a) => Uri -> Get m (V.Vector a)
vector uri = do
    ui <- urid Uri.vector
    next (==ui) $ \sz -> do
        elSize' <- word32_
        elType' <- word32_

        elType <- urid uri
        when (elType' /= elType) $
            throw $ ParseException
                  $ "Invalid vector element type " ++ show elType' ++ ", expected " ++ show elType ++ " (" ++ uri ++ ")"

        let elSize = sizeOf (undefined :: a)
        when (elSize' /= elSize) $
            throw $ ParseException
                  $ "Invalid vector element size " ++ show elSize' ++ ", expected " ++ show elSize

        b <- bytes sz
        let (fp, bo, _) = B.toForeignPtr b
            n = (sz - headerSize) `div` elSize
            (o, r) = fromIntegral bo `divMod` elSize
        if r == 0
            then if o == 0
                 then return $! V.unsafeFromForeignPtr (castForeignPtr fp) (fromIntegral o) (fromIntegral n)
                 else return $! V.unsafeFromForeignPtr (castForeignPtr fp) (fromIntegral o) (fromIntegral n)
            else throw $ ParseException
                       $ "Whoops, unaligned vector offset, call your doctor"

vectorFloat :: (C.MonadThrow m, Uri.Map m) => Get m (V.Vector Float)
vectorFloat = vector Uri.float

vectorDouble :: (C.MonadThrow m, Uri.Map m) => Get m (V.Vector Double)
vectorDouble = vector Uri.double

-- instance (C.MonadThrow m, Uri.Map m) => FromAtom m Int32 where
--     fromAtom = int32
-- instance (C.MonadThrow m, Uri.Map m) => FromAtom m Word32 where
--     fromAtom = int32

data ObjectId = Blank Urid Urid | Resource Urid Urid

data Object = Object {
    objId :: ObjectId
  , objProps :: H.HashMap (Maybe Urid, Urid) BL.ByteString
  }

(.:?) :: (Uri.Map m, FromAtom m a) => Object -> Uri -> Get m (Maybe a)
obj .:? uri = do
    ui <- urid uri
    case H.lookup (Nothing, ui) (objProps obj) of
        Nothing -> return Nothing
        Just b -> liftM Just $ lift $ CL.sourceList (BL.toChunks b) C.$$ fromAtom

class Uri.Map m => FromObject m a where
    fromObject :: Object -> Get m a

propertyBody :: Monad m => Get m ((Maybe Urid, Urid), BL.ByteString)
propertyBody = do
    c <- word32_
    k <- word32_
    b <- atomBytes
    return $! ((if c == 0 then Nothing else Just c, k), b)

object :: (C.MonadThrow m, Uri.Map m, FromObject m a) => Get m a
object = do
    resourceId <- urid Uri.resource
    blankId <- urid Uri.blank
    next (\x -> x == resourceId || x == blankId) $ \sz -> do
        id' <- word32_
        otype' <- word32_
        let n = sz - 2 * sizeOf (undefined :: Word32)
        ps <- CB.isolate (fromIntegral n) C.=$ props []
        return undefined
    where
        props ps = do
            m <- CL.peek
            case m of
                Nothing -> return ps
                Just _ -> do
                    p <- propertyBody
                    return $! p:ps

decode :: (C.MonadThrow m, Uri.Map m) => Get m a -> B.ByteString -> m a
-- decode p b = P.eitherResult $ P.feed (P.parse p b) ""
decode a b = -- runST
           -- $ runExceptionT
            -- C.runResourceT
           CL.sourceList [b]
          C.$$ a

-- Data.Conduit.runResourceT $ Uri.runPureMap $ (encode (toAtom [(pi::Double), 1, 2, 3])) >>= Sound.LV2.Atom.Get.decode (Sound.LV2.Atom.Get.vectorDouble)