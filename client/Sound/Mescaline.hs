{-# LANGUAGE GeneralizedNewtypeDeriving #-}
module Sound.Mescaline (
    Engine
  , EngineT
  , runEngineT
  , start
  , stop
) where

import           Bindings.Sound.Mescaline
import           Control.Applicative
import           Control.Monad (liftM)
import           Control.Monad.IO.Class (MonadIO(..))
import           Control.Monad.Trans.Class (MonadTrans)
import           Control.Monad.Trans.Resource (MonadResource(..), MonadThrow(..))
import qualified Control.Monad.Trans.State.Strict as S
import qualified Data.Text as Text
import           Foreign.C
import           Foreign.Ptr
import qualified Sound.LV2.Uri as Uri

newtype Engine = Engine { c'engine :: Ptr C'Mescaline_Engine }

newtype EngineT m a = EngineT { unEngineT :: S.StateT Engine m a }
                        deriving (Applicative, Functor, Monad, MonadIO, MonadResource, MonadThrow, MonadTrans)

instance MonadIO m => Uri.Map (EngineT m) where
    map uri = EngineT $ do
        c'urid <- S.gets c'engine >>=
                    liftIO . withCString (Uri.toString uri)
                           . c'Mescaline_Engine_mapUri
        return $! Uri.fromWord32 (fromIntegral c'urid)

instance MonadIO m => Uri.Unmap (EngineT m) where
    unmap urid = EngineT $ do
        c'str <- S.gets c'engine >>= liftIO . flip c'Mescaline_Engine_unmapUri (fromIntegral (Uri.toWord32 urid))
        if c'str == nullPtr
        then return Nothing
        else liftM (Just . Uri.fromText . Text.pack) (liftIO (peekCAString c'str))

runEngineT :: (MonadIO m, MonadResource m) => EngineT m a -> m a
runEngineT a = do
    e <- liftIO c'Mescaline_Engine_new
    register (c'Mescaline_Engine_free e)
    S.evalStateT (unEngineT a) (Engine e)

c'lift1 :: MonadIO m => (Ptr C'Mescaline_Engine -> IO a) -> EngineT m a
c'lift1 f = EngineT $ S.gets c'engine >>= liftIO . f

start :: MonadIO m => EngineT m ()
start = c'lift1 c'Mescaline_Engine_start

stop :: MonadIO m => EngineT m ()
stop = c'lift1 c'Mescaline_Engine_stop
