{-# LANGUAGE DeriveDataTypeable, GeneralizedNewtypeDeriving #-}
module Sound.LV2.Atom.Parser where

import           Control.Applicative (Applicative)
import           Control.Exception (Exception(..), SomeException, assert)
import           Control.Monad
import           Control.Monad.IO.Class (MonadIO(..))
import           Control.Monad.Trans.Class (MonadTrans(..))
import qualified Control.Monad.Trans.Class as S
import           Control.Monad.Trans.Error
import qualified Control.Monad.Trans.State.Strict as S
import           Data.ByteString (ByteString)
import qualified Data.ByteString as B
import qualified Data.ByteString.Internal as B
import           Data.Conduit (MonadThrow(..))
import           Data.Text (Text)
import qualified Data.Text.Encoding as T
import           Data.Typeable (Typeable)
import           Data.Word (Word32)
import           Foreign.ForeignPtr
import           Foreign.Ptr
import           Foreign.Storable (Storable)
import qualified Foreign.Storable as S
import           Prelude hiding (take)
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

instance Error ParseException where
    noMsg = UnknownException
    strMsg = ParseError

newtype Parser m a = Parser { runParser :: ErrorT ParseException (S.StateT State m) a }
                        deriving (Applicative, Functor, Monad, MonadIO)

instance S.MonadTrans Parser where
    lift = Parser . S.lift . S.lift

instance Uri.Map m => Uri.Map (Parser m) where
    map = lift . Uri.map

run :: MonadThrow m => Parser m a -> B.ByteString -> m (a, State)
run p b = do
    (a, s) <- S.runStateT (runErrorT (runParser p)) (State b 0)
    case a of
        Left e -> monadThrow e
        Right a -> return (a, s)

parse :: MonadThrow m => Parser m a -> B.ByteString -> m a
parse p = liftM fst . run p

throw :: Monad m => ParseException -> Parser m a
throw = Parser . throwError

get :: Monad m => Parser m State
{-# INLINE get #-}
get = Parser $ S.lift S.get

gets :: Monad m => (State -> a) -> Parser m a
{-# INLINE gets #-}
gets = Parser . S.lift . S.gets

put :: Monad m => State -> Parser m ()
{-# INLINE put #-}
put = Parser . S.lift . S.put

take :: Monad m => Word32 -> Parser m ByteString
take n = do
    s <- get
    let ni = fromIntegral n
        (b, b') = B.splitAt ni (input s)
    unless (B.length b == ni)
        $ throw $ ParseError "Buffer underrun"
    put $ s { input = b'
            , consumed = consumed s + n }
    return b

takeUtf8 :: Monad m => Word32 -> Parser m Text
takeUtf8 n = do
    b <- take n
    case T.decodeUtf8' b of
        Left e -> throw $ ParseException $ toException e
        Right t -> return t

peek :: Monad m => Word32 -> Parser m ByteString
peek n = do
    s <- get
    let ni = fromIntegral n
        (b, b') = B.splitAt ni (input s)
    unless (B.length b == ni)
        $ throw $ ParseError "Buffer underrun"
    return b

atEnd :: Monad m => Parser m Bool
atEnd = liftM ((==0).B.length) $ gets input

drop :: Monad m => Word32 -> Parser m ()
drop n = do
    s <- get
    let ni = fromIntegral n
        b  = input s
    unless (B.length b >= ni)
        $ throw $ ParseError "Buffer underrun"
    put $ s { input = B.drop ni b
            , consumed = consumed s + n }

align :: Monad m => (Word32 -> Word32) -> Parser m ()
align f = do
    s <- get
    let c = consumed s
        b = input s
        c' = f c
        n = c' - c
        ni = fromIntegral n
    unless (c' >= c)
        $ throw $ ParseError "Invalid alignment"
    unless (B.length b >= ni)
        $ throw $ ParseError "Buffer underrun"
    put $ s { input = B.drop ni b
            , consumed = c' }

isolate :: MonadThrow m => Word32 -> Parser m a -> Parser m a
isolate n p = do
    b <- take n
    (a, s) <- lift $ run p b
    unless (B.null (input s))
        $ throw $ ParseError $ "isolate: left-over " ++ show (B.length (input s))
    return a

-- From Data.Serialize.Get (cereal)
castByteString :: (Storable a) => a -> ByteString -> a
{-# INLINE castByteString #-}
castByteString _ b =
    let (fp, o, _) = B.toForeignPtr b
        k p = S.peek (castPtr (p `plusPtr` o))
    in B.inlinePerformIO (withForeignPtr fp k)

takeStorable :: (Monad m, Storable a) => Parser m a
takeStorable = do
    a <- return undefined
    liftM (castByteString a) (take (fromIntegral (S.sizeOf a)))

peekStorable :: (Monad m, Storable a) => Parser m a
peekStorable = do
    a <- return undefined
    liftM (castByteString a) (peek (fromIntegral (S.sizeOf a)))
