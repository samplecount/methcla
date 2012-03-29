{-# LANGUAGE FlexibleInstances
           , GeneralizedNewtypeDeriving
           , MultiParamTypeClasses
           , OverloadedStrings
           , TypeSynonymInstances #-}
module Sound.LV2.Uri (
    Uri
  , fromText
  , fromString
  , toText
  , toString
  , Urid
  , fromWord32
  , toWord32
  , ToUrid(..)
  , Map(..)
  , Unmap(..)
  , base
  , int32
  , int64
  , bool
  , float
  , double
  , string
  , path
  , uri
  , tuple
  , vector
  -- , object
  , resource
  , blank
  , PureMap
  , runPureMap
  , evalPureMap
) where

import           Control.Monad
import           Control.Monad.Trans.Class (MonadTrans)
import           Control.Monad.Trans.Control (MonadTransControl)
import           Data.Hashable (Hashable)
import qualified Data.Map as Map
import           Data.String (IsString)
import           Data.Text (Text)
import qualified Data.Text as T
import           Data.Word
import qualified Control.Monad.Trans.State.Strict as S
import           Prelude hiding (map)

newtype Uri = Uri { toText :: Text } deriving (Eq, Hashable, IsString, Ord, Show)

fromText :: Text -> Uri
fromText = Uri

fromString :: String -> Uri
fromString = fromText . T.pack

toString :: Uri -> String
toString = T.unpack . toText

newtype Urid = Urid { toWord32 :: Word32 } deriving (Eq, Hashable, Ord, Show)

fromWord32 :: Word32 -> Urid
fromWord32 = Urid

class Map m => ToUrid m a where
    toUrid :: a -> m Urid

instance Map m => ToUrid m Uri where
    toUrid = map
instance Map m => ToUrid m Urid where
    toUrid = return

class Monad m => Map m where
    map :: Uri -> m Urid

class Monad m => Unmap m where
    unmap :: Urid -> m (Maybe Uri)

base :: Uri
base = fromText "http://lv2plug.in/ns/ext/atom"

prefix :: Text -> Uri
prefix s = fromText $ toText base `T.append` "#" `T.append` s

int32, int64, bool, float, double :: Uri
int32  = prefix "Int32"
int64  = prefix "Int64"
bool   = prefix "Bool"
float  = prefix "Float"
double = prefix "Double"

string, path, uri :: Uri
string = prefix "String"
path   = prefix "Path"
uri    = prefix "URI"

tuple, vector :: Uri
tuple  = prefix "Tuple"
vector = prefix "Vector"
-- object = prefix "Object"
resource = prefix "Resource"
blank = prefix "Blank"

-- For testing

type PureMap = S.StateT (Map.Map Uri Urid, Map.Map Urid Uri)
                        -- deriving (Monad, MonadTrans, MonadTransControl)

pureMap = id
unPureMap = id

instance Monad m => Map (PureMap m) where
    map u = pureMap $ do
        (mf, mt) <- S.get
        case Map.lookup u mf of
            Just ui -> return ui
            Nothing -> do
                let !ui = fromWord32 $ fromIntegral (Map.size mf) + 1
                    !mf' = Map.insert u ui mf
                    !mt' = Map.insert ui u mt
                S.put (mf', mt')
                return ui

instance Monad m => Unmap (PureMap m) where
    unmap ui = pureMap $ S.gets (Map.lookup ui . snd)

runPureMap :: PureMap m a -> m (a, (Map.Map Uri Urid, Map.Map Urid Uri))
runPureMap a = S.runStateT (unPureMap a) (Map.empty, Map.empty)

evalPureMap :: Monad m => PureMap m a -> m a
evalPureMap = liftM fst . runPureMap
