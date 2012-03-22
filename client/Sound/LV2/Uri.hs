{-# LANGUAGE FlexibleInstances, GeneralizedNewtypeDeriving, TypeSynonymInstances #-}
module Sound.LV2.Uri (
    Uri
  , Urid
  , Map(..)
  , base
  , int32
  , int64
  , bool
  , float
  , double
  , tuple
  , vector
  -- , object
  , resource
  , blank
  , PureMap
  , runPureMap
  , evalPureMap
) where

import Control.Monad
import Control.Monad.Trans.Class (MonadTrans)
import Control.Monad.Trans.Control (MonadTransControl)
import qualified Data.Map as Map
import Data.Word
import qualified Control.Monad.Trans.State.Strict as S

type Uri = String
type Urid = Word32

class Monad m => Map m where
    map :: Uri -> m Urid

class Monad m => Unmap m where
    unmap :: Urid -> m (Maybe Uri)

base :: Uri
base = "http://lv2plug.in/ns/ext/atom"

prefix :: String -> Uri
prefix s = base ++ "#" ++ s

int32, int64, bool, float, double :: Uri
int32  = prefix "Int32"
int64  = prefix "Int64"
bool   = prefix "Bool"
float  = prefix "Float"
double = prefix "Double"

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
                let !ui = fromIntegral (Map.size mf) + 1
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
