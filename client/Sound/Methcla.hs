-- Copyright 2012-2013 Samplecount S.L.
-- 
-- Licensed under the Apache License, Version 2.0 (the "License");
-- you may not use this file except in compliance with the License.
-- You may obtain a copy of the License at
-- 
--     http://www.apache.org/licenses/LICENSE-2.0
-- 
-- Unless required by applicable law or agreed to in writing, software
-- distributed under the License is distributed on an "AS IS" BASIS,
-- WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
-- See the License for the specific language governing permissions and
-- limitations under the License.

{-# LANGUAGE FlexibleContexts, GeneralizedNewtypeDeriving #-}
module Sound.Methcla (
    Engine
  , new
  , start
  , stop
  -- , request_
  , request
) where

import           Bindings.Sound.Methcla
import           Control.Applicative
import           Control.Concurrent
import           Control.Exception (throwIO)
import           Control.Monad (liftM)
import           Control.Monad.IO.Class (MonadIO(..))
import           Control.Monad.Trans.Reader (runReaderT)
import           Control.Monad.Trans.Class (MonadTrans(..))
-- import           Control.Monad.Trans.Resource (MonadResource(..), MonadThrow(..))
import qualified Control.Monad.Trans.State.Strict as S
import qualified Data.ByteString as B
import qualified Data.ByteString.Internal as B
import qualified Data.Text as Text
import           Data.Word (Word32)
import           Foreign.C
import           Foreign.ForeignPtr
import           Foreign.Ptr
import           Foreign.StablePtr
import           Foreign.Storable
import           Sound.LV2.Atom (FromAtom(..), ToAtom(..), encode, decode_)
import qualified Sound.LV2.Uri as Uri

data Engine = Engine {
    c'engine :: ForeignPtr C'Methcla_Engine
  }

new :: IO Engine
new = Engine <$> (newForeignPtr p'methcla_engine_free =<< c'methcla_engine_new nullPtr)

withEnginePtr :: Engine -> (Ptr C'Methcla_Engine -> IO a) -> IO a
withEnginePtr engine = withForeignPtr (c'engine engine)

mapUri :: Engine -> Uri.Uri -> IO Uri.Urid
mapUri engine uri = do
  c'urid <- withCString (Uri.toString uri) $ \c'uri -> 
              withEnginePtr engine (flip c'methcla_engine_map_uri c'uri)
  return $! Uri.fromWord32 (fromIntegral c'urid)

unmapUri :: Engine -> Uri.Urid -> IO (Maybe Uri.Uri)
unmapUri engine urid = do
  c'str <- withEnginePtr engine $
            flip c'methcla_engine_unmap_uri
                 (fromIntegral (Uri.toWord32 urid))
  if c'str == nullPtr
    then return Nothing
    else Just . Uri.fromText . Text.pack <$> peekCAString c'str

-- | Start the engine.
start :: Engine -> IO ()
start = flip withEnginePtr c'methcla_engine_start

-- | Stop the engine.
stop :: Engine -> IO ()
stop = flip withEnginePtr c'methcla_engine_stop

-- | Asynchronous request.
-- request_ :: (MonadIO m, MonadThrow m, ToAtom (EngineT m) a) => a -> EngineT m ()
-- request_ a = do
    -- e <- gets c'engine
    -- ba <- encode (toAtom a)
    -- liftIO $ do
        -- let (fp, o, _) = B.toForeignPtr ba
        -- withForeignPtr fp $ \p ->
            -- c'methcla_engine_request e (castPtr p) nullFunPtr nullPtr

-- | Synchronous request.
request :: (ToAtom a, FromAtom b) => Engine -> a -> IO b
request engine a = do
  -- Convert a to ByteString
  ba <- runReaderT (encode (toAtom a)) (mapUri engine)
  -- Get response ByteString
  -- Create destination MVar
  result <- newEmptyMVar
  handler <- mk'Methcla_Response_Handler $ \_ _ response -> do
    -- Get response atom size
    size <- peek (response `plusPtr` sizeOf (1::Word32)) :: IO Word32
    -- Create ByteString from response atom
    b <- B.packCStringLen (castPtr response, fromIntegral size)
    -- Put ByteString to reponse MVar
    putMVar result b
  -- Get ByteString pointer
  let (fp, o, _) = B.toForeignPtr ba
  -- Pass request to engine
  withForeignPtr fp $ \p ->
    withEnginePtr engine $ \enginePtr -> 
      c'methcla_engine_request
        enginePtr
        handler
        nullPtr
        (castPtr (p `plusPtr` o))
  -- Take response ByteString from MVar
  bb <- takeMVar result
  -- Decode response ByteString
  result <- runReaderT (decode_ fromAtom bb) (mapUri engine)
  case result of
    Left e -> throwIO e
    Right a -> return a

