module Sound.LV2.Atom.Object (
    Object
  , ObjectUri(..)
  , Key
  , Properties
  , uri, resourceId, rdfType, properties
  , fromList, blank, resource
  , lookup
  , elems
  , traverseWithKey
  , mapWithKey
) where

import           Control.Applicative
import qualified Data.HashMap.Strict as H
import           Data.Maybe (fromJust)
import qualified Data.Foldable as F
import qualified Data.Traversable as T
import           Prelude hiding (lookup)
import           Sound.LV2.Uri (Urid)

data ObjectUri = Blank | Resource deriving (Eq, Show)
type Key = (Urid, Urid)
type Properties a = H.HashMap Key a

data Object a = Object {
    uri :: ObjectUri
  , resourceId :: Urid
  , rdfType :: Urid
  , properties :: Properties a
  } deriving (Show)

fromList :: ObjectUri -> Urid -> Urid -> [(Key, a)] -> Object a
fromList ui ri rt = Object ui ri rt . H.fromList

blank :: Urid -> Urid -> [(Key, a)] -> Object a
blank = fromList Blank

resource :: Urid -> Urid -> [(Key, a)] -> Object a
resource = fromList Resource

modify :: (Properties a -> Properties b) -> Object a -> Object b
modify f obj = obj { properties = f (properties obj) }

modifyA :: Applicative f => (Properties a -> f (Properties b)) -> Object a -> f (Object b)
modifyA f obj = fmap (\ps -> obj { properties = ps }) (f (properties obj))

instance Functor Object where
    fmap = modify . fmap

lookup :: Key -> Object a -> Maybe a
lookup k = H.lookup k . properties

elems :: Object a -> [a]
elems = H.elems . properties

traverseWithKey :: Applicative f => (Key -> v1 -> f v2) -> Object v1 -> f (Object v2)
traverseWithKey = modifyA . H.traverseWithKey

mapWithKey :: (Key -> a -> b) -> Object a -> Object b
mapWithKey f = fromJust . traverseWithKey (\k -> Just . f k)

instance F.Foldable Object where
    foldMap f = F.foldMap f . properties

instance T.Traversable Object where
    traverse f obj = fmap (\ps -> obj { properties = ps }) (T.traverse f (properties obj))
