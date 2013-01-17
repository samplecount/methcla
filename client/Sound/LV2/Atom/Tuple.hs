module Sound.LV2.Atom.Tuple (
    Tuple
  , fromList
  , toList
) where

import qualified Data.Foldable as F
import qualified Data.Traversable as T
import           Sound.LV2.Uri

newtype Tuple a = Tuple { toList :: [a] }
                    deriving (Functor, F.Foldable, Show, T.Traversable)

fromList :: [a] -> Tuple a
fromList = Tuple
