{-# LANGUAGE OverloadedStrings, TemplateHaskell #-}
import           Control.Applicative
import           Control.Concurrent (readMVar)
import           Control.Exception.Lifted
import           Control.Monad
import           Control.Monad.IO.Class (MonadIO, liftIO)
import           Control.Monad.Trans.Class (lift)
import qualified Control.Monad.Trans.Resource as Resource
import qualified Control.Monad.Trans.RWS.Strict as S
import           Data.Aeson (FromJSON(..), ToJSON(..), Value(..), (.=), (.:), (.:?), object)
import qualified Data.Aeson as J
import qualified Data.Aeson.Types as J
import qualified Data.Attoparsec.ByteString.Char8 as A
import qualified Data.ByteString as BS
import qualified Data.ByteString.Char8 as BS8
import qualified Data.ByteString.Lazy as BL
import qualified Data.ByteString.Lazy.Char8 as BC
import           Data.Conduit (($$), (=$=))
import qualified Data.Conduit as C
import qualified Data.Conduit.Attoparsec as C
import qualified Data.Conduit.Binary as CB
import qualified Data.Conduit.Cereal as C
import qualified Data.Conduit.List as CL
import qualified Data.Conduit.Network as C
import qualified Data.HashMap.Strict as H
import           Data.Lens.Common
import           Data.Lens.Template
import           Data.Serialize.Get (getWord32be)
import           Data.Serialize.Put (putLazyByteString, putWord32be, runPut)
import           Data.Text (Text)
import qualified Data.Text as Text
import qualified Data.Text.IO as Text
import qualified Filesystem as FS
import qualified Filesystem.Path.CurrentOS as FS
import qualified Network.Socket as N
import qualified Sound.SC3.Server.Monad as SC
import qualified Sound.SC3.Server.Process as SC
import qualified Sound.SC3.Server.Process.Monad as SCM
import qualified Streamero.Darkice as Darkice
import           Streamero.Readline (sourceReadline)
import qualified Streamero.Process as SProc
import qualified Streamero.Jack as SJack
import           System.Console.CmdArgs.Explicit
import           System.Exit (ExitCode(..), exitWith)
import           System.IO
import           System.Posix.Env (getEnv, setEnv, unsetEnv)
import qualified System.Process as Proc
import           System.Unix.Directory (withTemporaryDirectory)

import Debug.Trace

catMaybes :: Monad m => C.Conduit (Maybe a) m a
catMaybes = do
    v <- C.await
    case v of
        Nothing -> return ()
        Just Nothing -> catMaybes
        Just (Just a) -> C.yield a >> catMaybes

-- | Chunk input into messages delimited by 32 bit integer byte counts.
message :: C.MonadThrow m => C.Conduit BS.ByteString m BC.ByteString
message = C.sequence $ do
    w <- C.sinkGet getWord32be
    case w of
        Left e -> lift . C.monadThrow . C.ParseError [] $ e
        Right n -> CB.take (fromIntegral n)

-- | Flatten messages into the output stream, prepending them with a 32 bit integer byte count.
unmessage :: Monad m => C.Conduit BC.ByteString m BS.ByteString
unmessage = CL.map $ \b -> runPut $ putWord32be (fromIntegral (BC.length b)) >> putLazyByteString b

-- | Flatten messages into lines in the output stream.
unlines :: Monad m => C.Conduit BC.ByteString m BS.ByteString
unlines = CL.map (BS.concat . BL.toChunks . flip BC.snoc '\n')

-- | Convert JSON values to Haskell values.
fromJson :: (C.MonadThrow m, FromJSON a) => C.Conduit J.Value m a
fromJson = loop where
    loop = do
        next <- C.await
        case next of
            Nothing -> return ()
            Just v -> case J.fromJSON v of
                        J.Success a -> C.yield a >> loop
                        J.Error e   -> lift . C.monadThrow . C.ParseError [] $ e

-- | Convert Haskell values to JSON values.
toJson :: (MonadIO m, ToJSON a) => C.Conduit a m BC.ByteString
toJson = CL.map J.encode

-- | Parse JSON messages into JSON values.
parseJsonMessage :: C.MonadThrow m => C.Conduit BC.ByteString m J.Value
parseJsonMessage = loop where
    loop = do
        next <- C.await
        case next of
            Nothing -> return ()
            Just b -> case A.parse J.json (BS.concat $ BC.toChunks $ b) of
                        A.Done _ v    -> C.yield v >> loop
                        A.Fail _ cs e -> lift . C.monadThrow . C.ParseError cs $ e
                        A.Partial _   -> lift $ C.monadThrow C.DivergentParser

-- | Parse JSON or whitespace.
jsonOrWhite :: A.Parser (Maybe J.Value)
jsonOrWhite = (Just <$> J.json) <|> (A.many1 A.space >> return Nothing)

-- | Parse a stream of JSON messages into JSON values.
parseJsonStream :: C.MonadThrow m => C.Conduit BS.ByteString m J.Value
parseJsonStream = C.sequence (C.sinkParser jsonOrWhite) =$= catMaybes

type SessionId = String
type LocationId = Int
type SoundId = Int

data Coord = Coord { latitude :: Double, longitude :: Double } deriving (Eq, Show)

instance FromJSON Coord where
    parseJSON (Object v) = Coord <$> v .: "latitude" <*> v .: "longitude"
    parseJSON _          = mzero

data Sound =
    SoundFile {
        path :: FilePath
      , loop :: Bool
    }
    deriving (Show)

instance FromJSON Sound where
    parseJSON (Object v) = do
        t <- v .: "type" :: J.Parser Text
        case t of
            "sample" -> SoundFile <$> v .: "path" <*> v .: "loop"
            _        -> mzero
    parseJSON _          = mzero

data Listener = Listener {
    listenerPosition :: Coord
  } deriving (Show)

data Location = Location {
    position :: Coord
  , radius :: Double
  , locationSounds :: [SoundId]
  } deriving (Show)

data Request =
    Quit
  | AddListener SessionId Coord
  | RemoveListener SessionId
  | UpdateListener SessionId (Listener -> Listener)
  | AddSound SoundId Sound
  | AddLocation LocationId Coord Double [SoundId]
  | UpdateLocation LocationId (Location -> Location)

instance FromJSON Request where
    parseJSON o@(Object v) = do
        t <- v .: "request" :: J.Parser Text
        case t of
            "Quit"           -> pure Quit
            "AddListener"    -> AddListener <$> v .: "id" <*> v .: "position"
            "RemoveListener" -> RemoveListener <$> v .: "id"
            "UpdateListener" -> UpdateListener <$> v .: "id" <*> (maybe id (\p l -> l { listenerPosition = p }) <$> v .:? "position")
            "AddSound"       -> AddSound <$> v .: "id" <*> J.parseJSON o
            "AddLocation"    -> AddLocation <$> v .: "id" <*> v .: "position" <*> v .: "radius" <*> v .: "sounds"
            "UpdateLocation" -> UpdateLocation <$> v .: "id" <*> foldM (\f -> fmap ((.)f)) id
                                                                    [ maybe id (\x s -> s { position = x }) <$> v .:? "position"
                                                                    , maybe id (\x s -> s { radius = x })   <$> v .:? "radius"
                                                                    , maybe id (\x s -> s { locationSounds = x })   <$> v .:? "sounds" ]
            _                -> mzero
    parseJSON _ = mzero

data Response = Ok | Error String

instance ToJSON Response where
    toJSON Ok = object [ "response" .= ("Ok" :: Text) ]
    toJSON (Error e) = object [ "response" .= ("Error" :: Text), "message" .= e ]

data Environment = Env {
    _maxNumListeners :: Int
  , _jackServerName :: String
  } deriving (Show)

data State = State {
    _sounds    :: H.HashMap SoundId Sound
  , _locations :: H.HashMap LocationId Location
  , _listeners :: H.HashMap SessionId Listener
  } deriving (Show)

$( makeLenses [''Environment, ''State] )

makeState :: State
makeState = State H.empty H.empty H.empty

type AppT = S.RWST Environment () State

-- | Engine state machine.
engine :: MonadIO m => Request -> AppT m (Maybe Response)
engine Quit = return Nothing
engine request = do
    response <- case request of
        AddSound id sound -> do
            S.modify (modL sounds (H.insert id sound))
            return Ok
        AddLocation id pos radius sounds -> do
            let x = Location pos radius sounds
            S.modify (modL locations (H.insert id x))
            return Ok
        UpdateLocation id f -> do
            S.modify (modL locations (H.adjust f id))
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
        Quit -> error "BUG"
    S.get >>= liftIO . hPutStrLn stderr . show
    return $! Just response

-- | Engine conduit mapping requests to responses.
engineC :: MonadIO m => Environment -> State -> C.Conduit Request m Response
engineC r s = C.conduitState
            s
            (\s i -> do
                (r, s', _) <- S.runRWST (engine i) r s
                case r of
                    Nothing -> return $ C.StateFinished Nothing []
                    Just a  -> return $ C.StateProducing s' [a])
            (\_ -> return [])

data Options = Options {
    _help :: Bool
  , _socket :: Maybe FilePath
  , _audioDevice :: Maybe String
  , _opt_maxNumListeners :: Int
  } deriving (Show)

defaultOptions :: Options
defaultOptions = Options {
    _help = False
  , _socket = Nothing
  , _audioDevice = Nothing
  , _opt_maxNumListeners = 1
  }

$( makeLenses [''Options] )

makeEnv :: Options -> String -> Environment
makeEnv opts = Env (opt_maxNumListeners ^$ opts)

arguments :: Mode Options
arguments =
    mode "streamero-sound-engine" defaultOptions "Streamero Sound Engine v0.1"
         (flagArg (upd socket . Just) "SOCKET") $
         [ flagHelpSimple (setL help True)
         , flagReq ["audio-device", "d"] (upd audioDevice . Just) "STRING" "Audio device"
         , flagReq ["max-num-listeners", "n"] (upd opt_maxNumListeners . read) "NUMBER" "Maximum number of listeners"
         ]
    where
        upd what x = Right . setL what x

withSC :: Environment -> String -> SC.Server a -> IO a
withSC env clientName =
    SCM.withSynth
        SC.defaultServerOptions {
            SC.numberOfInputBusChannels = 2
          , SC.numberOfOutputBusChannels = 2 * (maxNumListeners ^$ env) }
        SC.defaultRTOptions {
            SC.hardwareDeviceName = Just $ (jackServerName ^$ env) ++ ":" ++ clientName }
        SC.defaultOutputHandler { SC.onPutString = logStrLn "SC"
                                , SC.onPutError = logStrLn "SC" }

withUnixSocket :: String -> (N.Socket -> IO a) -> IO a
withUnixSocket file = bracket
    (do { s <- N.socket N.AF_UNIX N.Stream 0
        ; N.connect s (N.SockAddrUnix file)
        ; return s
        })
    N.sClose

logStrLn :: String -> String -> IO ()
logStrLn tag s = putStrLn $ "[" ++ tag ++ "] " ++ s

logLn :: String -> IO ()
logLn = logStrLn "ENGINE"

someException :: SomeException -> SomeException
someException = id

main :: IO ()
main = do
    opts <- processArgs arguments
    if help ^$ opts
        then print $ helpText [] HelpFormatDefault arguments
        else do
            e <- try $ withTemporaryDirectory "streamero_" $ \tmpDir_ -> do
                let tmpDir = FS.decodeString tmpDir_
                    jackOptions = (SJack.defaultOptions (FS.encodeString $ FS.basename tmpDir)) { SJack.driver = "coreaudio" }
                -- Set HOME directory for jack to find its config file
                {-setEnv "HOME" tmpDir_ True-}
                {-FS.writeTextFile (FS.append tmpDir ".jackdrc") (Text.unwords . map Text.pack $ ["/usr/local/bin/jackd"] ++ SJack.optionList jackOptions)-}
                {-FS.readTextFile (FS.append tmpDir ".jackdrc") >>= Text.putStrLn-}
                {-unsetEnv "JACK_NO_START_SERVER"-}
                {-setEnv "JACK_START_SERVER" "1" True-}
                logLn $ "Starting with temporary directory " ++ tmpDir_
                SJack.withJack (logStrLn "JACK") "jackdmp" jackOptions $ \jackServerName -> do
                    logLn $ "Jack server name: " ++ jackServerName
                    void $ SJack.withPatchBay jackServerName "patchbay" [(("supercollider","out_1"),("darkice","left")),(("supercollider","out_2"),("darkice","right"))] $ do
                        let env = makeEnv opts jackServerName
                            run source sink = withSC env "supercollider" $ do
                                let darkiceCfgFile = FS.append tmpDir "darkice.cfg"
                                    darkiceCfg = Darkice.defaultConfig {
                                                    Darkice.jackClientName = Just "darkice"
                                                  , Darkice.mountPoint = "soundscape" }
                                    darkiceOpts = Darkice.defaultOptions {
                                                    Darkice.configFile = Just darkiceCfgFile }
                                liftIO $ do
                                    FS.writeTextFile darkiceCfgFile $ Darkice.configText darkiceCfg
                                    SProc.withProcess (logStrLn "STREAMER") (Darkice.createProcess "darkice" darkiceOpts) $
                                        source =$= engineC env makeState $$ sink
                        const $ lift $ case socket ^$ opts of
                            Nothing ->
                                -- Readline interface
                                let source = sourceReadline "> " =$= CL.map BS8.pack =$= parseJsonStream =$= fromJson
                                    sink = toJson =$= unlines =$= CB.sinkHandle stdout
                                in run source sink
                            Just socketFile ->
                                -- Socket interface
                                withUnixSocket socketFile $ \s ->
                                    let source = C.sourceSocket s =$= message =$= parseJsonMessage =$= fromJson
                                        sink = toJson =$= unmessage =$= C.sinkSocket s
                                    in run source sink
            case e of
                Left exc -> do
                    putStrLn $ "Exception caught: " ++ show (someException exc)
                    exitWith $ ExitFailure 1
                Right _ ->
                    exitWith ExitSuccess
