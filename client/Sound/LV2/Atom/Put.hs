{-# LANGUAGE FlexibleContexts, FlexibleInstances, GeneralizedNewtypeDeriving, MultiParamTypeClasses #-}
module Sound.LV2.Atom.Put (
    Put
  , ToAtom(..)
  , PutObject
  , object, blank, resource
  , property, property_
  , encode
) where

import           Blaze.ByteString.Builder as Builder
import           Blaze.ByteString.Builder.Char.Utf8
import           Blaze.ByteString.Builder.Int
import           Blaze.ByteString.Builder.Word
import           Control.Applicative
import           Control.Exception (assert)
import           Control.Monad
import qualified Control.Monad.Trans.State.Strict as S
import qualified Control.Monad.Trans.Class as S
import           Data.Bits
import qualified Data.ByteString as B
import qualified Data.ByteString.Lazy as BL
import qualified Data.Map as Map
import           Data.Monoid
import           Data.Int
import           Data.Word
import qualified Data.Vector.Storable as V
import           Data.Text (Text)
import qualified Data.Text as Text
import qualified Data.Text.Encoding as Text
import           Sound.LV2.Atom.Class
import           Sound.LV2.Atom.Object (Object)
import qualified Sound.LV2.Atom.Object as Object
import           Sound.LV2.Atom.Primitive
import           Sound.LV2.Atom.Util
import           Sound.LV2.Uri (Uri, Urid, ToUrid(..))
import qualified Sound.LV2.Uri as Uri

newtype Put m a = Put { runPut :: S.StateT Builder m a }
                    deriving (Applicative, Functor, Monad, S.MonadTrans)

class ToAtom m a where
    toAtom :: a -> Put m ()

instance Uri.Map m => Uri.Map (Put m) where
    map = S.lift . Uri.map

append :: Monad m => Builder -> Put m ()
append b = Put $ S.modify (`mappend` b)

putUrid :: Monad m => Urid -> Put m ()
putUrid = append . fromWord32host . Uri.toWord32

header :: Monad m => Urid -> Word32 -> Put m ()
header urid size = append (fromWord32host size `mappend` fromWord32host (Uri.toWord32 urid))

zeroPad :: Monad m => Word32 -> Put m ()
zeroPad n = append (go n)
    where go 0 = mempty
          go n = fromWrite (writeWord8 0) `mappend` go (n-1)

build :: Monad m => Urid -> Word32 -> Builder -> Put m ()
build urid size builder = header urid size >> append builder >> zeroPad (padding size)

build' :: Monad m => Urid -> Builder -> Put m ()
build' urid builder = build urid n (fromLazyByteString b)
    where b = toLazyByteString builder
          n = int64ToWord32 (BL.length b) -- TODO: Error handling

embed :: Monad m => Put m () -> Put m Builder
embed a = Put $ S.lift $ S.execStateT (runPut a) mempty

encode :: Monad m => Put m () -> m B.ByteString
encode a = liftM toByteString $ S.execStateT (runPut a) mempty

primitive :: (Uri.Map m, Atom a, Primitive a) => a -> Put m ()
primitive a = Uri.map (atomUri a) >>= \ui -> build ui (sizeOf a) (toBuilder a)

instance Uri.Map m => ToAtom m Int32 where
    toAtom = primitive

instance Uri.Map m => ToAtom m Int64 where
    toAtom = primitive

instance Uri.Map m => ToAtom m Word32 where
    toAtom = toAtom . toInt32
        where toInt32 :: Word32 -> Int32
              toInt32 = fromIntegral

instance Uri.Map m => ToAtom m Bool where
    toAtom = primitive

instance Uri.Map m => ToAtom m Float where
    toAtom = primitive

instance Uri.Map m => ToAtom m Double where
    toAtom = primitive

-- putText :: Monad m => Text -> Put m ()
-- putText = append . fromText

stringOf :: Uri.Map m => Uri -> Text -> Put m ()
stringOf uri t = Uri.map uri >>= \ui -> build ui (fromIntegral (Text.length t + 1)) (Builder.fromByteString (Text.encodeUtf8 t) `mappend` fromWrite (writeWord8 0))

instance Uri.Map m => ToAtom m Text where
    toAtom = stringOf Uri.string

int64ToWord32 :: Int64 -> Word32
int64ToWord32 n = assert (n < fromIntegral (maxBound :: Word32)) (fromIntegral n)

putProperty :: (Monad m, ToAtom m a) => Urid -> Urid -> a -> Put m ()
putProperty k c v = do
    putUrid k
    putUrid c
    toAtom v

putObject :: Uri.Map m => Object (Put m ()) -> Put m ()
putObject obj = do
    b <- embed $ do
        putUrid (Object.resourceId obj)
        putUrid (Object.rdfType obj)
        sequence_ (Object.elems obj)
    ui <- case Object.uri obj of
            Object.Blank -> Uri.map Uri.blank
            Object.Resource -> Uri.map Uri.resource
    build' ui b

instance Uri.Map m => ToAtom m (Object (Put m ())) where
    toAtom = putObject
instance (Uri.Map m, ToAtom m a) => ToAtom m (Object a) where
    toAtom = putObject . Object.mapWithKey (uncurry putProperty)

newtype PutObject m a = PutObject (Put m a)
                            deriving (Monad, Uri.Map)

object :: (Uri.Map m, ToUrid (Put m) r, ToUrid (Put m) t) => Object.ObjectUri -> r -> t -> PutObject m () -> Put m ()
object u r t (PutObject props) = do
    b <- embed $ do
        putUrid =<< toUrid r
        putUrid =<< toUrid t
        props
    ui <- case u of
            Object.Blank -> Uri.map Uri.blank
            Object.Resource -> Uri.map Uri.resource
    build' ui b

blank :: (Uri.Map m, ToUrid (Put m) r, ToUrid (Put m) t) => r -> t -> PutObject m () -> Put m ()
blank = object Object.Blank

resource :: (Uri.Map m, ToUrid (Put m) r, ToUrid (Put m) t) => r -> t -> PutObject m () -> Put m ()
resource = object Object.Resource

property :: (Uri.Map m, ToUrid (Put m) k, ToUrid (Put m) c, ToAtom m a) => k -> c -> a -> PutObject m ()
property k c a = PutObject $ do
    uk <- toUrid k
    uc <- toUrid c
    putProperty uk uc a

property_ :: (Uri.Map m, ToUrid (Put m) k, ToAtom m a) => k -> a -> PutObject m ()
property_ k = property k (Uri.fromWord32 0)

tuple :: Uri.Map m => [Put m ()] -> Put m ()
tuple as = do
    ui <- Uri.map Uri.tuple
    build' ui =<< embed (sequence_ as)

-- Tuple
instance (Uri.Map m) => ToAtom m [Put m ()] where
    toAtom = tuple
instance (Uri.Map m, ToAtom m a1, ToAtom m a2) => ToAtom m (a1, a2) where
    toAtom (a1, a2) = tuple [toAtom a1, toAtom a2]
instance (Uri.Map m, ToAtom m a1, ToAtom m a2, ToAtom m a3) => ToAtom m (a1, a2, a3) where
    toAtom (a1, a2, a3) = tuple [toAtom a1, toAtom a2, toAtom a3]

-- Vector
instance (Uri.Map m, Primitive a, ToAtom m a) => ToAtom m [a] where
    toAtom [] = error "empty list"
    toAtom (a:as) = do
        -- Get bytestring for first atom
        b <- liftM toLazyByteString $ embed (toAtom a)
        let na = headerSize + sizeOf a
            -- Bytestring without padding:
            -- element_size <> element_type <> element_data[0]
            ba = BL.take (fromIntegral na) b
            -- element_data[1..]
            bas = toLazyByteString (mconcat (map toBuilder as))
        let n = na + int64ToWord32 (BL.length bas)
        ui <- Uri.map Uri.vector
        build ui n (fromLazyByteString ba `mappend` fromLazyByteString bas)
