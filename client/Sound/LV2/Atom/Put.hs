{-# LANGUAGE FlexibleInstances #-}
module Sound.LV2.Atom.Put where

import Blaze.ByteString.Builder
import Blaze.ByteString.Builder.Int
import Blaze.ByteString.Builder.Word
import Control.Monad.Trans.State
import Control.Monad.Trans.Class
import Data.Monoid
import Data.Int
import Data.Word
import Data.Text.Lazy (Text)
import           Sound.LV2.Uri (Uri, Urid)
import qualified Sound.LV2.Uri as Uri

lv2AtomUri = "http://lv2plug.in/ns/ext/atom"
uri_Int32 = lv2AtomUri ++ "#Int32"
uri_Int64 = lv2AtomUri ++ "#Int64"

data PutState m = PutState {
    uris :: Uri.Map m
  , builder :: Builder
  }

type Put m a = StateT (PutState m) m a

-- -- | The 'Value' typeclass represents types that can be rendered
-- -- into valid atom syntax.

class Value a where
    put :: Monad m => a -> Put m Atom

data Atom = Atom

urid :: Monad m => Uri -> Put m Urid
urid u = gets uris >>= lift . flip Uri.map u

append :: Monad m => Builder -> Put m ()
append b = modify (\s -> s { builder = builder s `mappend` b })

atom :: Monad m => Put m Atom
atom = return Atom

pad :: Monad m => Word32 -> Put m ()
pad n = append (go n)
    where go 0 = mempty
          go n = fromWrite (writeWord8 0) `mappend` go (n-1)

header :: Monad m => Word32 -> Urid -> Put m ()
header size urid = append (fromWord32host size `mappend` fromWord32host urid)

header' :: Monad m => Uri -> Word32 -> Put m ()
header' uri size = urid uri >>= header size

build :: Monad m => Uri -> Word32 -> Builder -> Put m Atom
build uri size builder = header' uri size >> append builder >> pad (8 - size) >> atom

instance Value Int32 where
    put = build uri_Int32 4 . fromInt32host

instance Value Int64 where
    put = build uri_Int64 8 . fromInt64host

float :: Float -> Put m Atom
float = undefined

double :: Double -> Put m Atom
double = undefined

-- urid :: Urid -> Put Atom
-- string :: Text -> Put Atom

instance Monad m => Value [Put m Atom] where
    put as = do
        s <- get
        s' <- lift $ execStateT (sequence as) s
        return Atom

-- tuple :: [Atom] -> Put m Atom
-- tuple = undefined
-- 
-- vector :: Value a => [a] -> Put m Atom
-- vector = undefined
