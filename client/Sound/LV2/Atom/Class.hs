{-# LANGUAGE MultiParamTypeClasses #-}
module Sound.LV2.Atom.Class (
    Atom(..)
) where

import Data.Int
import Data.Word
import Sound.LV2.Uri as Uri

class Atom a where
    atomUri :: a -> Uri

instance Atom Int32 where
    atomUri _ = Uri.int32

instance Atom Int64 where
    atomUri _ = Uri.int64

instance Atom Word32 where
    atomUri _ = atomUri (undefined::Int32)

instance Atom Word64 where
    atomUri _ = atomUri (undefined::Int64)

instance Atom Bool where
    atomUri _ = Uri.bool

instance Atom Float where
    atomUri _ = Uri.float

instance Atom Double where
    atomUri _ = Uri.double
