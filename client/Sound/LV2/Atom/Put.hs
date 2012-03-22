{-# LANGUAGE FlexibleInstances, MultiParamTypeClasses #-}
module Sound.LV2.Atom.Put where

import           Blaze.ByteString.Builder
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
import           Data.Monoid
import           Data.Int
import           Data.Word
import           Data.Text.Lazy (Text)
-- import           Foreign.Storable (Storable, sizeOf)
import           Sound.LV2.Atom.Primitive
import           Sound.LV2.Atom.Util
import           Sound.LV2.Uri (Uri, Urid)
import qualified Sound.LV2.Uri as Uri

type Put m a = S.StateT Builder m a

-- -- | The 'ToAtom' typeclass represents types that can be rendered
-- -- into valid atom syntax.

newtype Atom m = Atom { unAtom :: Put m () }

class Uri.Map m => ToAtom m a where
    toAtom :: a -> Atom m

-- urid :: (Uris -> Urid) -> Put Urid
-- urid f = fmap f (gets uris)
urid :: Uri.Map m => Uri -> Put m Urid
urid = S.lift . Uri.map

putUrid :: Monad m => Urid -> Put m ()
putUrid = append . fromWord32host

append :: Monad m => Builder -> Put m ()
append b = S.modify (`mappend` b)

header :: Uri.Map m => Uri -> Word32 -> Put m ()
header uri size = urid uri >>= \urid -> append (fromWord32host size `mappend` fromWord32host urid)

pad :: Uri.Map m => Word32 -> Put m ()
pad n = append (go n)
    where go 0 = mempty
          go n = fromWrite (writeWord8 0) `mappend` go (n-1)

build :: Uri.Map m => Uri -> Word32 -> Builder -> Put m ()
build uri size builder = header uri size >> append builder >> pad (padding size)

build' :: Uri.Map m => Uri -> Builder -> Put m ()
build' uri builder = build uri n (fromLazyByteString b)
    where b = toLazyByteString builder
          n = int64ToWord32 (BL.length b) -- TODO: Error handling

primitive :: (Primitive a, Uri.Map m) => Uri -> a -> Atom m
primitive uri a = Atom $ build uri (sizeOf a) (toBuilder a)

embed :: Uri.Map m => Put m () -> Put m Builder
-- embed a = gets uris >>= \us -> return $ builder $ execState a (PutState us mempty)
embed a = S.lift $ S.execStateT a mempty

encode :: Uri.Map m => Atom m -> m B.ByteString
encode a = do
    s <- S.execStateT (unAtom a) mempty
    return $! toByteString s

instance Uri.Map m => ToAtom m Int32 where
    toAtom = primitive Uri.int32

instance Uri.Map m => ToAtom m Int64 where
    toAtom = primitive Uri.int64

instance Uri.Map m => ToAtom m Bool where
    toAtom = primitive Uri.bool

instance Uri.Map m => ToAtom m Float where
    toAtom = primitive Uri.float

instance Uri.Map m => ToAtom m Double where
    toAtom = primitive Uri.double

-- urid :: Urid -> Put Atom
-- string :: Text -> Put Atom

int64ToWord32 :: Int64 -> Word32
int64ToWord32 n = assert (n < fromIntegral (maxBound :: Word32)) (fromIntegral n)

newtype Property m = Property { unProperty :: Put m () }

property :: (Uri.Map m, ToAtom m a) => Maybe Uri -> Uri -> a -> Property m
property c k v = Property $ do
    putUrid =<< maybe (return 0) urid c
    putUrid =<< urid k
    unAtom (toAtom v)

putObject :: Uri.Map m => Uri -> Urid -> Uri -> [Property m] -> Put m ()
putObject rtype id otype ps = do
    b <- embed $ do
        putUrid id
        putUrid =<< urid otype
        mapM_ unProperty ps
    build' rtype b

blank :: Uri.Map m => Urid -> Uri -> [Property m] -> Atom m
blank id otype = Atom . putObject Uri.blank id otype

resource :: Uri.Map m => Urid -> Uri -> [Property m] -> Atom m
resource id otype = Atom . putObject Uri.resource id otype

-- Tuple
instance (Functor m, Uri.Map m) => ToAtom m [Atom m] where
    toAtom as = Atom $ build' Uri.tuple =<< embed (mapM_ unAtom as)

-- Vector
instance (Primitive a, Functor m, Uri.Map m, ToAtom m a) => ToAtom m [a] where
    toAtom [] = error "empty list"
    toAtom (a:as) = Atom $ do
        -- Get bytestring for first atom
        b <- toLazyByteString <$> embed (unAtom (toAtom a))
        let na = headerSize + sizeOf a
            -- Bytestring without padding:
            -- element_size <> element_type <> element_data[0]
            ba = BL.take (fromIntegral na) b
            -- element_data[1..]
            bas = toLazyByteString (mconcat (map toBuilder as))
        let n = na + int64ToWord32 (BL.length bas)
        build Uri.vector n (fromLazyByteString ba `mappend` fromLazyByteString bas)
