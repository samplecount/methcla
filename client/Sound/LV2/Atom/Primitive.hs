module Sound.LV2.Atom.Primitive (
    Primitive(..)
  , castByteString
) where

import           Blaze.ByteString.Builder
import           Blaze.ByteString.Builder.Int (fromInt32host, fromInt64host)
import           Blaze.ByteString.Builder.Word (fromWord32host, fromWord64host)
import           Control.Exception (assert)
import           Data.Binary.IEEE754 (doubleToWord, floatToWord)
import           Data.ByteString (ByteString)
import qualified Data.ByteString as B
import qualified Data.ByteString.Internal as B
import           Data.Int (Int32, Int64)
import           Data.Word (Word32, Word64)
import           Foreign.ForeignPtr
import           Foreign.Ptr
import           Foreign.Storable (Storable)
import qualified Foreign.Storable as S

-- From Data.Serialize.Get (cereal)
castByteString :: (Storable a) => Word32 -> ByteString -> a
{-# INLINE castByteString #-}
castByteString n b =
    let (fp, o, _) = B.toForeignPtr (assert (B.length b >= fromIntegral n) b)
        k p = S.peek (castPtr (p `plusPtr` o))
    in B.inlinePerformIO (withForeignPtr fp k)

class Primitive a where
    sizeOf :: a -> Word32
    toBuilder :: a -> Builder
    fromByteString :: ByteString -> a

instance Primitive Int32 where
    sizeOf _ = 4
    toBuilder = fromInt32host
    fromByteString = castByteString (sizeOf (undefined :: Int32))

instance Primitive Word32 where
    sizeOf _ = 4
    toBuilder = fromWord32host
    fromByteString = castByteString (sizeOf (undefined :: Word32))

instance Primitive Int64 where
    sizeOf _ = 8
    toBuilder = fromInt64host
    fromByteString = castByteString (sizeOf (undefined :: Int64))

instance Primitive Word64 where
    sizeOf _ = 8
    toBuilder = fromWord64host
    fromByteString = castByteString (sizeOf (undefined :: Word64))

instance Primitive Bool where
    sizeOf _ = sizeOf (undefined :: Int32)
    toBuilder = toBuilder . toInt32
        where
            toInt32 :: Bool -> Int32
            toInt32 True = 1
            toInt32 False = 0
    fromByteString = fromInt32 . castByteString (sizeOf (undefined :: Int32))
        where
            fromInt32 :: Int32 -> Bool
            fromInt32 0 = False
            fromInt32 _ = True

instance Primitive Float where
    sizeOf _ = 4
    toBuilder = fromWord32host . floatToWord
    fromByteString = castByteString (sizeOf (undefined :: Float))

instance Primitive Double where
    sizeOf _ = 8
    toBuilder = fromWord64host . doubleToWord
    fromByteString = castByteString (sizeOf (undefined :: Double))
