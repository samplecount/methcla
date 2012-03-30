{-# LANGUAGE FlexibleContexts, GeneralizedNewtypeDeriving #-}
module Sound.Mescaline (
    Engine
  , EngineT
  , runEngineT
  , start
  , stop
  , request_
  , request
) where

import           Bindings.Sound.Mescaline
import           Control.Applicative
import           Control.Concurrent
import           Control.Monad (liftM)
import           Control.Monad.IO.Class (MonadIO(..))
import           Control.Monad.Trans.Class (MonadTrans(..))
import           Control.Monad.Trans.Resource (MonadResource(..), MonadThrow(..))
import qualified Control.Monad.Trans.State.Strict as S
import qualified Data.ByteString.Internal as B
import qualified Data.Text as Text
import           Foreign.C
import           Foreign.ForeignPtr
import           Foreign.Ptr
import           Foreign.StablePtr
import           Sound.LV2.Atom (FromAtom(..), ToAtom(..), encode, decode)
import qualified Sound.LV2.Uri as Uri

data Engine = Engine {
    c'engine :: Ptr C'Mescaline_Engine
  , c'handler :: C'Mescaline_HandleResponse
  }

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

get :: Monad m => EngineT m Engine
get = EngineT S.get

gets :: Monad m => (Engine -> a) -> EngineT m a
gets = EngineT . S.gets

free :: Engine -> IO ()
free e = do
    c'Mescaline_Engine_free (c'engine e)
    freeHaskellFunPtr (c'handler e)

handleResponse :: Ptr C'LV2_Atom -> Ptr () -> IO ()
handleResponse _ _ = return ()

runEngineT :: (MonadIO m, MonadResource m) => EngineT m a -> m a
runEngineT a = do
    s <- liftIO $ do
        e <- c'Mescaline_Engine_new
        h <- mk'Mescaline_HandleResponse handleResponse
        return $! Engine e h
    register (free s)
    S.evalStateT (unEngineT a) s

c'lift1 :: MonadIO m => (Ptr C'Mescaline_Engine -> IO a) -> EngineT m a
c'lift1 f = gets c'engine >>= liftIO . f

start :: MonadIO m => EngineT m ()
start = c'lift1 c'Mescaline_Engine_start

stop :: MonadIO m => EngineT m ()
stop = c'lift1 c'Mescaline_Engine_stop

-- | Asynchronous request.
request_ :: (MonadIO m, MonadThrow m, ToAtom (EngineT m) a) => a -> EngineT m ()
request_ a = do
    e <- gets c'engine
    ba <- encode (toAtom a)
    liftIO $ do
        let (fp, o, _) = B.toForeignPtr ba
        withForeignPtr fp $ \p -> c'Mescaline_Engine_request e (castPtr p) nullFunPtr nullPtr

-- | Synchronous request.
request :: (MonadIO m, MonadThrow m, ToAtom (EngineT m) a, FromAtom (EngineT m) b) => a -> EngineT m b
request a = do
    ba <- encode (toAtom a)
    Engine e h <- get
    liftIO $ do
        m <- newEmptyMVar
        c'm <- newStablePtr m
        let (fp, o, _) = B.toForeignPtr ba
        withForeignPtr fp $ \p -> c'Mescaline_Engine_request e (castPtr p) h (castStablePtrToPtr c'm)
        b <- takeMVar m
        freeStablePtr c'm
        return b
