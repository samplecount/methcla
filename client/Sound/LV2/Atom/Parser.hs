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

{-# LANGUAGE DeriveDataTypeable, GeneralizedNewtypeDeriving #-}
module Sound.LV2.Atom.Parser (
    Parser
  , ParseException(..)
  , parse
  , take
  , takeUtf8
  , atEnd
  , peek
  , drop
  , align
  , isolate
  , takeStorable
  , peekStorable
) where

import           Control.Applicative (Applicative)
import           Control.Exception (Exception(..), SomeException, assert)
import           Control.Monad
import           Control.Monad.IO.Class (MonadIO(..))
import           Control.Monad.Trans.Class (MonadTrans(..))
import qualified Control.Monad.Trans.Class as S
import           Control.Monad.Trans.Resource (MonadThrow(..))
import qualified Control.Monad.Trans.State.Strict as S
import           Data.ByteString (ByteString)
import qualified Data.ByteString as B
import qualified Data.ByteString.Internal as B
import           Data.Text (Text)
import qualified Data.Text.Encoding as T
import           Data.Typeable (Typeable)
import           Data.Word (Word32)
import           Foreign.ForeignPtr
import           Foreign.Ptr
import           Foreign.Storable (Storable)
import qualified Foreign.Storable as S
import           Prelude hiding (drop, take)
import qualified Sound.LV2.Uri as Uri

data State = State {
    input ::  {-# UNPACK #-} !B.ByteString
  , consumed :: {-# UNPACK #-} !Word32
  } deriving (Show)

data ParseException =
    UnknownException
  | ParseError String
  | ParseException SomeException
  deriving (Show, Typeable)

instance Exception ParseException

newtype Parser m a = Parser { runParser :: S.StateT State m a }
                        deriving (Applicative, Functor, Monad, MonadIO)

instance S.MonadTrans Parser where
    lift = Parser . S.lift

toParseException :: Exception e => e -> Maybe ParseException
toParseException = fromException . toException

instance MonadThrow m => MonadThrow (Parser m) where
    monadThrow e = S.lift $ monadThrow (maybe (toException e) toException (toParseException e))

instance Uri.Map m => Uri.Map (Parser m) where
    map = lift . Uri.map

parse :: MonadThrow m => Parser m a -> B.ByteString -> m (a, B.ByteString)
parse p b = do
    (a, s) <- S.runStateT (runParser p) (State b 0)
    return (a, input s)

parseError :: MonadThrow m => String -> Parser m a
parseError = monadThrow . ParseError

parseErrorAt :: MonadThrow m => String -> Parser m a
parseErrorAt s = do
    n <- gets consumed
    parseError (s ++ " (at byte " ++ show n ++ ")")

bufferUnderrun :: MonadThrow m => Parser m a
bufferUnderrun = parseErrorAt "Buffer underrun"

-- Private API

get :: Monad m => Parser m State
{-# INLINE get #-}
get = Parser $ S.get

gets :: Monad m => (State -> a) -> Parser m a
{-# INLINE gets #-}
gets = Parser . S.gets

put :: Monad m => State -> Parser m ()
{-# INLINE put #-}
put = Parser . S.put

-- Public API

take :: MonadThrow m => Word32 -> Parser m ByteString
take n = do
    s <- get
    let ni = fromIntegral n
        (b, b') = B.splitAt ni (input s)
    unless (B.length b == ni)
        bufferUnderrun
    put $ s { input = b'
            , consumed = consumed s + n }
    return b

takeUtf8 :: MonadThrow m => Word32 -> Parser m Text
takeUtf8 n = do
    b <- take n
    case T.decodeUtf8' b of
        Left e -> monadThrow $ ParseException $ toException e
        Right t -> return t

atEnd :: Monad m => Parser m Bool
atEnd = liftM ((==0).B.length) $ gets input

peek :: MonadThrow m => Word32 -> Parser m ByteString
peek n = do
    s <- get
    let ni = fromIntegral n
        (b, b') = B.splitAt ni (input s)
    unless (B.length b == ni)
        bufferUnderrun
    return b

drop :: MonadThrow m => Word32 -> Parser m ()
drop n = do
    s <- get
    let ni = fromIntegral n
        b  = input s
    unless (B.length b >= ni)
        bufferUnderrun
    put $ s { input = B.drop ni b
            , consumed = consumed s + n }

align :: MonadThrow m => (Word32 -> Word32) -> Parser m ()
align f = do
    s <- get
    let c = consumed s
        b = input s
        c' = f c
        n = c' - c
        ni = fromIntegral n
    unless (n >= 0)
        $ parseErrorAt $ "Invalid alignment: " ++ show n
    unless (B.length b >= ni)
        bufferUnderrun
    put $ s { input = B.drop ni b
            , consumed = c' }

isolate :: MonadThrow m => Word32 -> Parser m a -> Parser m a
isolate n p = do
    b <- take n
    (a, b') <- lift $ parse p b
    unless (B.null b')
        $ parseErrorAt $ "Left-over " ++ show (B.length b') ++ " of " ++ show (B.length b) ++ " bytes"
    return a

-- From Data.Serialize.Get (cereal)
castByteString :: (Storable a) => a -> ByteString -> a
{-# INLINE castByteString #-}
castByteString _ b =
    let (fp, o, _) = B.toForeignPtr b
        k p = S.peek (castPtr (p `plusPtr` o))
    in B.inlinePerformIO (withForeignPtr fp k)

takeStorable :: (MonadThrow m, Storable a) => Parser m a
takeStorable = do
    a <- return undefined
    liftM (castByteString a) (take (fromIntegral (S.sizeOf a)))

peekStorable :: (MonadThrow m, Storable a) => Parser m a
peekStorable = do
    a <- return undefined
    liftM (castByteString a) (peek (fromIntegral (S.sizeOf a)))
