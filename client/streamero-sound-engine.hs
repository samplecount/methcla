{-# LANGUAGE OverloadedStrings, TemplateHaskell #-}
import           Control.Applicative
import           Control.Monad
import           Control.Monad.IO.Class (MonadIO, liftIO)
import           Control.Monad.Trans.Class (lift)
import qualified Control.Monad.Trans.RWS.Strict as S
import           Data.Aeson (encode, json)
import           Data.Aeson (FromJSON(..), ToJSON(..), Value(..), (.=), (.:), (.:?), object)
import qualified Data.Aeson as J
import qualified Data.Aeson.Types as J
import qualified Data.Attoparsec.ByteString.Char8 as A
import qualified Data.Attoparsec.Combinator as A
import qualified Data.ByteString.Lazy as BL
import qualified Data.ByteString.Lazy.Char8 as BC
import qualified Data.ByteString as BS
import           Data.Conduit (($$), ($=), (=$), (=$=))
import qualified Data.Conduit as C
import qualified Data.Conduit.Attoparsec as C
import qualified Data.Conduit.Binary as C
import qualified Data.Conduit.List as C
import qualified Data.HashMap.Strict as H
import           Data.Lens.Common
import           Data.Lens.Template
import           Data.Text (Text)
import qualified Sound.SC3.Server.Process as SC
import qualified Sound.SC3.Server.Process.Monad as SC
import           System.Console.CmdArgs.Explicit
import           System.IO

import Debug.Trace

-- Adapted from [1]
conduitParser :: (C.MonadThrow m) =>
                        A.Parser b
                     -> C.Conduit BS.ByteString m b
conduitParser p0 = C.conduitState newParser push close
  where
    newParser = A.parse (A.many1 p0)
    push parser c
        | BS.null c = return $ C.StateFinished Nothing []
    push parser c = do
        case A.feed (parser c) BS.empty of
            A.Done leftover xs
                | BS.null leftover ->
                    return $ C.StateProducing newParser xs
                | otherwise ->
                    return $ C.StateProducing (newParser . BS.append leftover) xs
            A.Fail _ contexts msg -> C.monadThrow $ C.ParseError contexts msg
            A.Partial p -> return $ C.StateProducing p []
    close parser =
        case parser BS.empty of
            A.Done _leftover xs -> return xs
            A.Fail _ contexts msg -> C.monadThrow $ C.ParseError contexts msg
            A.Partial _ -> return [] -- A partial parse when closing is not an error

{-fromJson' :: C.MonadThrow m => C.Conduit BS.ByteString m J.Value-}
{-fromJson' = C.sequence $ do-}
    {-x <- C.sinkParser json-}
    {-C.dropWhile (== 10)-}
    {-return x-}

{-bytesToJSON = C.sequence (C.sinkParser json)-}
{-bytesToJSON = conduitParser json-}
bytesToJSON = fromJsonOrWhite
jsonSource = C.sourceHandle stdin $= bytesToJSON

fromJsonC :: (C.MonadThrow m, FromJSON a) => C.Conduit J.Value m a
fromJsonC = do
    v <- C.await
    case v of
        Nothing -> return ()
        Just v -> do
            case J.fromJSON v of
                J.Success a -> C.yield a
                J.Error e   -> lift $ C.monadThrow (C.ParseError [] e)
            fromJsonC

toJson = C.map (BS.concat . BL.toChunks . flip BC.snoc '\n' . J.encode)
jsonSink = toJson =$ C.sinkHandle stdout

jsonOrWhite :: A.Parser (Maybe J.Value)
jsonOrWhite = (Just <$> json) <|> (A.many1 A.space >> return Nothing)

catMaybes :: Monad m => C.Conduit (Maybe a) m a
catMaybes = do
    v <- C.await
    case v of
        Nothing -> return ()
        Just Nothing -> catMaybes
        Just (Just a) -> C.yield a >> catMaybes

fromJsonOrWhite :: C.MonadThrow m => C.Conduit BS.ByteString m J.Value
fromJsonOrWhite = C.sequence (C.sinkParser jsonOrWhite) =$= catMaybes

data Options = Options {
    _help :: Bool
  , _audioDevice :: Maybe String
  , _maxNumListeners :: Int
  } deriving (Show)

defaultOptions :: Options
defaultOptions = Options {
    _help = False
  , _audioDevice = Nothing
  , _maxNumListeners = 1
  }

$( makeLenses [''Options] )
 
arguments :: Mode Options
arguments =
    mode "streamero-sound-engine" defaultOptions "Streamero Sound Engine v0.1"
         (flagArg (const . Left) "") $
         [ flagHelpSimple (setL help True)
         , flagReq ["audio-device", "d"] (upd audioDevice . Just) "STRING" "Audio device"
         , flagReq ["max-num-listeners", "n"] (upd maxNumListeners . read) "NUMBER" "Maximum number of listeners"
         ]
    where
        upd what x = Right . setL what x

type SessionId = String
type SourceId = Int
type SoundId = Int
data Point = Point { x :: Double, y :: Double, z :: Double } deriving (Eq, Show)

data Sound =
    SoundFile {
        path :: FilePath
      , loop :: Bool
    }
    deriving (Show)

data Listener = Listener {
    position :: Point
  } deriving (Show)

data Source = Source {
    sourcePosition :: Point
  , sourceRadius :: Double
  , sourceSound :: Maybe SoundId
  } deriving (Show)

data Request =
    AddListener SessionId Point
  | RemoveListener SessionId
  | UpdateListener SessionId (Listener -> Listener)
  | AddSound SoundId Sound
  | AddSource SourceId Point Double
  | UpdateSource SourceId (Source -> Source)

instance FromJSON Point where
    parseJSON (Object v) = Point <$>
                              v .: "x" <*>
                              v .: "y" <*>
                              v .: "z"
    parseJSON _ = mzero

instance FromJSON Sound where
    parseJSON (Object v) = SoundFile <$> v .: "path" <*> v .: "loop"
    parseJSON _ = mzero

instance FromJSON Request where
    parseJSON (Object v) = do
        t <- v .: "type" :: J.Parser Text
        case t of
            "AddListener"    -> AddListener <$> v .: "id" <*> v .: "position"
            "RemoveListener" -> RemoveListener <$> v .: "id"
            "UpdateListener" -> UpdateListener <$> v .: "id" <*> (maybe id (\p l -> l { position = p }) <$> v .:? "position")
            "AddSound"       -> AddSound <$> v .: "id" <*> v .: "sound"
            "AddSource"      -> AddSource <$> v .: "id" <*> v .: "position" <*> v .: "radius"
            "UpdateSource"   -> UpdateSource <$> v .: "id" <*> foldM (\f -> fmap ((.)f)) id
                                                                [ maybe id (\x s -> s { sourcePosition = x }) <$> v .:? "position"
                                                                , maybe id (\x s -> s { sourceRadius = x }) <$> v .:? "radius"
                                                                , maybe id (\x s -> s { sourceSound = Just x }) <$> v .:? "sound" ]
            _                -> mzero
    parseJSON _ = mzero

data Response = Ok | Error String

instance ToJSON Response where
    toJSON Ok = object [ "type" .= ("Ok" :: Text) ]
    toJSON (Error e) = object [ "type" .= ("Error" :: Text), "message" .= e ]

data State = State {
    _sounds    :: H.HashMap SoundId Sound
  , _sources   :: H.HashMap SourceId Source
  , _listeners :: H.HashMap SessionId Listener
  } deriving (Show)

$( makeLenses [''State] )

makeState = State H.empty H.empty H.empty

type AppT = S.RWST Int () State

app :: MonadIO m => Request -> AppT m Response
app request = do
    response <- case request of
        AddSound id sound -> do
            S.modify (modL sounds (H.insert id sound))
            return Ok
        AddSource id pos radius -> do
            let s = Source pos radius Nothing
            S.modify (modL sources (H.insert id s))
            return Ok
        UpdateSource id f -> do
            S.modify (modL sources (H.adjust f id))
            return Ok
        AddListener id pos -> do
            let l = Listener pos
            S.modify (modL listeners (H.insert id l))
            return Ok
        RemoveListener id -> do
            S.modify (modL listeners (H.delete id))
            return Ok
        UpdateListener id f -> do
            S.modify (modL listeners (H.adjust f id))
            return Ok
    S.get >>= liftIO . print
    return response

appC :: MonadIO m => Int -> State -> C.Conduit Request m Response
appC r s = C.conduitState
            s
            (\s i -> do
                (a, s', _) <- S.runRWST (app i) r s
                return $! C.StateProducing s' [a])
            (\_ -> return [])

withSC opts =
    SC.withInternal
        SC.defaultServerOptions
        SC.defaultRTOptions {
            SC.hardwareDeviceName = audioDevice ^$ opts }
        SC.defaultOutputHandler

main = do
    {-hSetBuffering stdin LineBuffering-}
    {-hSetBuffering stdout LineBuffering-}
    opts <- processArgs arguments
    if help ^$ opts
        then print $ helpText [] HelpFormatDefault arguments
        else withSC opts $ C.runResourceT ((jsonSource $= fromJsonC) $= (appC (maxNumListeners ^$ opts) makeState) $$ jsonSink)

-- [1] https://github.com/boothead/conduit/blob/90161072b3bb80317016b4ba565eb521f6d6dfb4/attoparsec-conduit/Data/Conduit/Attoparsec.hs

