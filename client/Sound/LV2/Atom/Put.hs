{-# LANGUAGE FlexibleInstances, MultiParamTypeClasses #-}
module Sound.LV2.Atom.Put where

import           Blaze.ByteString.Builder
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
-- import           Foreign.Storable (Storable, sizeOf)
import           Sound.LV2.Atom.Object (Object)
import qualified Sound.LV2.Atom.Object as Object
import           Sound.LV2.Atom.Primitive
-- import           Sound.LV2.Atom.Tuple (Tuple)
import           Sound.LV2.Atom.Util
import           Sound.LV2.Uri (Uri, Urid)
import qualified Sound.LV2.Uri as Uri

data Uris = Uris {
    uri_Int32  :: Urid
  , uri_Int64  :: Urid
  , uri_Bool   :: Urid
  , uri_Float  :: Urid
  , uri_Double :: Urid
  , uri_Tuple  :: Urid
  , uri_Vector :: Urid
  }

type Put m a = S.StateT Builder m a

-- -- | The 'ToAtom' typeclass represents types that can be rendered
-- -- into valid atom syntax.

newtype Atom m = Atom { unAtom :: Put m () }

class ToAtom m a where
    toAtom :: a -> Atom m

-- urid :: (Uris -> Urid) -> Put Urid
-- urid f = fmap f (gets uris)
urid :: Uri.Map m => Uri -> Put m Urid
urid = S.lift . Uri.map

append :: Monad m => Builder -> Put m ()
append b = S.modify (`mappend` b)

putUrid :: Monad m => Urid -> Put m ()
putUrid = append . fromWord32host

header :: Monad m => Urid -> Word32 -> Put m ()
header urid size = append (fromWord32host size `mappend` fromWord32host urid)

pad :: Monad m => Word32 -> Put m ()
pad n = append (go n)
    where go 0 = mempty
          go n = fromWrite (writeWord8 0) `mappend` go (n-1)

build :: Monad m => Urid -> Word32 -> Builder -> Put m ()
build urid size builder = header urid size >> append builder >> pad (padding size)

build' :: Monad m => Urid -> Builder -> Put m ()
build' urid builder = build urid n (fromLazyByteString b)
    where b = toLazyByteString builder
          n = int64ToWord32 (BL.length b) -- TODO: Error handling

embed :: Monad m => Put m () -> Put m Builder
-- embed a = gets uris >>= \us -> return $ builder $ execState a (PutState us mempty)
embed a = S.lift $ S.execStateT a mempty

encode :: Monad m => Atom m -> m B.ByteString
encode a = do
    s <- S.execStateT (unAtom a) mempty
    return $! toByteString s

primitive :: (Primitive a, Monad m) => Urid -> a -> Atom m
primitive urid a = Atom $ build urid (sizeOf a) (toBuilder a)

primitive_ :: (Uri.Map m, Primitive a) => Uri -> a -> Atom m
primitive_ uri a = Atom $ urid uri >>= \ui -> unAtom $ primitive ui a

instance Uri.Map m => ToAtom m Int32 where
    toAtom = primitive_ Uri.int32

instance Uri.Map m => ToAtom m Int64 where
    toAtom = primitive_ Uri.int64

instance Uri.Map m => ToAtom m Bool where
    toAtom = primitive_ Uri.bool

instance Uri.Map m => ToAtom m Float where
    toAtom = primitive_ Uri.float

instance Uri.Map m => ToAtom m Double where
    toAtom = primitive_ Uri.double

-- putText :: Monad m => Text -> Put m ()
-- putText = append . fromText

stringOf :: Uri.Map m => Uri -> Text -> Atom m
stringOf uri t = Atom $ urid uri >>= \ui -> build ui (fromIntegral (Text.length t + 1)) (fromText t)

instance Uri.Map m => ToAtom m Text where
    toAtom = stringOf Uri.string

-- urid :: Urid -> Put Atom
-- string :: Text -> Put Atom

int64ToWord32 :: Int64 -> Word32
int64ToWord32 n = assert (n < fromIntegral (maxBound :: Word32)) (fromIntegral n)

newtype Property m = Property { putProperty :: Put m () }

property :: (Monad m, ToAtom m a) => Urid -> Urid -> a -> Property m
property c k v = Property $ do
    putUrid c
    putUrid k
    unAtom (toAtom v)

object :: Uri.Map m => Object (Property m) -> Atom m
object obj = Atom $ do
    b <- embed $ do
        putUrid (Object.resourceId obj)
        putUrid (Object.rdfType obj)
        mapM_ putProperty (Object.elems obj)
    urid <- case Object.uri obj of
                Object.Blank -> urid Uri.blank
                Object.Resource -> urid Uri.resource
    build' urid b

tuple :: Uri.Map m => [Atom m] -> Atom m
tuple as = Atom $ do
    ui <- urid Uri.tuple
    b <- embed (mapM_ unAtom as)
    build' ui b

-- Tuple
instance (Uri.Map m) => ToAtom m [Atom m] where
    toAtom = tuple
instance (Uri.Map m, ToAtom m a1, ToAtom m a2) => ToAtom m (a1, a2) where
    toAtom (a1, a2) = tuple [toAtom a1, toAtom a2]
instance (Uri.Map m, ToAtom m a1, ToAtom m a2, ToAtom m a3) => ToAtom m (a1, a2, a3) where
    toAtom (a1, a2, a3) = tuple [toAtom a1, toAtom a2, toAtom a3]

-- Vector
instance (Uri.Map m, Primitive a, ToAtom m a) => ToAtom m [a] where
    toAtom [] = error "empty list"
    toAtom (a:as) = Atom $ do
        -- Get bytestring for first atom
        b <- liftM toLazyByteString $ embed (unAtom (toAtom a))
        let na = headerSize + sizeOf a
            -- Bytestring without padding:
            -- element_size <> element_type <> element_data[0]
            ba = BL.take (fromIntegral na) b
            -- element_data[1..]
            bas = toLazyByteString (mconcat (map toBuilder as))
        let n = na + int64ToWord32 (BL.length bas)
        ui <- urid Uri.vector
        build ui n (fromLazyByteString ba `mappend` fromLazyByteString bas)

-- Object
instance (Uri.Map m) => ToAtom m (Object (Property m)) where
    toAtom = object

instance (Uri.Map m, ToAtom m a) => ToAtom m (Object a) where
    toAtom = object . Object.mapWithKey (uncurry property)
