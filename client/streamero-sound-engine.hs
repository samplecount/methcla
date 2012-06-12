{-# LANGUAGE ExistentialQuantification
           , FlexibleInstances
           , OverloadedStrings
           , TemplateHaskell
           , TypeSynonymInstances #-}
import           Control.Applicative
import           Control.Arrow (first)
import           Control.Category ((.))
import           Control.Concurrent (forkIO, myThreadId, threadDelay)
import qualified Control.Concurrent.Chan as Chan
import qualified Control.Concurrent.MVar as MVar
import qualified Control.Concurrent.STM as STM
import           Control.Exception.Lifted
import           Control.Failure (Failure, failure)
import           Control.Monad
import           Control.Monad.IO.Class (MonadIO, liftIO)
import           Control.Monad.Trans.Class (lift)
import           Data.Aeson (FromJSON(..), ToJSON(..), Value(..), (.=), (.:), (.:?), object)
import qualified Data.Aeson as J
import qualified Data.Aeson.Types as J
import qualified Data.Attoparsec.ByteString.Char8 as A
import qualified Data.Bits as Bits
import qualified Data.ByteString as BS
import qualified Data.ByteString.Lazy as BL
import qualified Data.ByteString.Lazy.Char8 as BC
import           Data.Conduit (($$), (=$=))
import qualified Data.Conduit as C
import qualified Data.Conduit.Attoparsec as C
import qualified Data.Conduit.Binary as CB
import qualified Data.Conduit.Cereal as C
import qualified Data.Conduit.List as CL
import qualified Data.Conduit.Network as C
import qualified Data.Foldable as F
import qualified Data.HashMap.Strict as H
import           Data.Lens.Common
import           Data.Lens.Template
import qualified Data.List as List
import           Data.Serialize.Get (getWord32be)
import           Data.Serialize.Put (putLazyByteString, putWord32be, runPut)
import           Data.Text (Text)
import qualified Data.Traversable as T
import           Data.Word (Word64)
import qualified Filesystem as FS
import qualified Filesystem.Path.CurrentOS as FS
import qualified Network.Socket as N
import           Network.URL (URL)
import qualified Network.URL as URL
import           Prelude hiding ((.), catch)
import qualified Reactive.Banana as R
import           Sound.OpenSoundControl (immediately)
import qualified Sound.OpenSoundControl as OSC
import qualified Sound.SC3.UGen as SC
import qualified Sound.SC3.Server.Allocator as SC
import qualified Sound.SC3.Server.Monad as SC
import qualified Sound.SC3.Server.Monad.Command as SC
import qualified Sound.SC3.Server.Monad.Process as SC
import qualified Sound.SC3.Server.Monad.Request as SC
import qualified Streamero.Darkice as Darkice
import qualified Streamero.Jack as SJack
import qualified Streamero.Process as SProc
{-import           Streamero.Readline (sourceReadline)-}
import           Streamero.Signals (ignoreSignals, catchSignals)
import qualified Streamero.SoundFile as SF
import           System.Console.CmdArgs.Explicit
import           System.IO
import           System.Posix.Signals (keyboardSignal)
import           System.Process (ProcessHandle)
import           System.Unix.Directory (withTemporaryDirectory)

catMaybes :: Monad m => C.Conduit (Maybe a) m a
catMaybes = do
    v <- C.await
    case v of
        Nothing -> return ()
        Just Nothing -> catMaybes
        Just (Just a) -> C.yield a >> catMaybes

printC :: (Show a, MonadIO m) => C.Conduit a m a
printC = do
    v <- C.await
    case v of
        Nothing -> return ()
        Just a -> liftIO (print a) >> C.yield a >> printC

-- | Chunk input into messages delimited by 32 bit integer byte counts.
message :: C.MonadThrow m => C.Conduit BS.ByteString m BC.ByteString
message = C.sequence $ do
    n <- C.sinkGet getWord32be
    CB.take (fromIntegral n)

-- | Flatten messages into the output stream, prepending them with a 32 bit integer byte count.
unmessage :: Monad m => C.Conduit BC.ByteString m BS.ByteString
unmessage = CL.map $ \b -> runPut $ putWord32be (fromIntegral (BC.length b)) >> putLazyByteString b

-- | Flatten messages into lines in the output stream.
unlines :: Monad m => C.Conduit BC.ByteString m BS.ByteString
unlines = CL.map (BS.concat . BL.toChunks . flip BC.snoc '\n')

-- | Convert JSON values to Haskell values.
fromJson :: (C.MonadThrow m, FromJSON a) => C.Conduit J.Value m a
fromJson = go where
    go = do
        next <- C.await
        case next of
            Nothing -> return ()
            Just v -> case J.fromJSON v of
                        J.Success a -> C.yield a >> go
                        J.Error e   -> lift . C.monadThrow . C.ParseError [] $ e

-- | Convert Haskell values to JSON values.
toJson :: (MonadIO m, ToJSON a) => C.Conduit a m BC.ByteString
toJson = CL.map J.encode

-- | Parse JSON messages into JSON values.
parseJsonMessage :: C.MonadThrow m => C.Conduit BC.ByteString m J.Value
parseJsonMessage = go where
    go = do
        next <- C.await
        case next of
            Nothing -> return ()
            Just b -> case A.parse J.json (BS.concat $ BC.toChunks $ b) of
                        A.Done _ v    -> C.yield v >> go
                        A.Fail _ cs e -> lift . C.monadThrow . C.ParseError cs $ e
                        A.Partial _   -> lift $ C.monadThrow C.DivergentParser

-- | Parse JSON or whitespace.
jsonOrWhite :: A.Parser (Maybe J.Value)
jsonOrWhite = (Just <$> J.json) <|> (A.many1 A.space >> return Nothing)

-- | Parse a stream of JSON messages into JSON values.
parseJsonStream :: C.MonadThrow m => C.Conduit BS.ByteString m J.Value
parseJsonStream = C.sequence (C.sinkParser jsonOrWhite) =$= catMaybes

type SoundscapeId = String
type SessionId = String
type LocationId = Int
type SoundId = Int

data Coord = Coord { latitude :: Double, longitude :: Double } deriving (Eq, Show)

instance FromJSON Coord where
    parseJSON (Object v) = Coord <$> v .: "latitude" <*> v .: "longitude"
    parseJSON _          = mzero

distance :: Coord -> Coord -> Double
distance c1 c2 = earthRadius * ahaversin h
    where
        earthRadius = 6371e3
        sqr x = x * x
        haversin = sqr . sin . (/2)
        ahaversin = (*2) . asin . sqrt
        deg2rad = (/180) . (*pi)
        h = haversin (deg2rad (latitude c2) - deg2rad (latitude c1)) +
               cos (deg2rad (latitude c1)) * cos (deg2rad (latitude c2))
                   * haversin (deg2rad (longitude c2) - deg2rad (longitude c1))

data Sound =
    SoundFile {
        path :: FilePath
      , loop :: Bool
    }
    deriving (Show)

data SoundFileInfo = SoundFileInfo {
    sampleRate :: Int
  , numChannels :: Int
  , numFrames :: Word64
  } deriving (Show)

instance FromJSON Sound where
    parseJSON (Object v) = do
        t <- v .: "type" :: J.Parser Text
        case t of
            "sample" -> SoundFile <$> v .: "path" <*> v .: "loop"
            _        -> mzero
    parseJSON _          = mzero

type ListenerId = SessionId

data Listener = Listener {
    listenerPosition :: Coord
  , streamURL :: URL
  } deriving (Show)

data Location = Location {
    position :: Coord
  , radius :: Double
  , locationSounds :: [SoundId]
  } deriving (Show)

data Request =
    Quit
  {-| Init SoundscapeId String String-}
  | AddListener ListenerId Coord URL
  | RemoveListener ListenerId
  | UpdateListener ListenerId (Listener -> Listener)
  | AddSound SoundId Sound
  | AddLocation LocationId Coord Double [SoundId]
  | RemoveLocation LocationId
  | UpdateLocation LocationId (Location -> Location)

parseURL a =
    case URL.importURL a of
        Nothing -> fail $ "Invalid URL " ++ a
        Just url -> return url

instance FromJSON Request where
    parseJSON o@(Object v) = do
        t <- v .: "request" :: J.Parser Text
        case t of
            "Quit"           -> pure Quit
            {-"Init"           -> Init <$> v .: "soundscape" <*> v .: "streamingServer" v .: "streamingPassword"-}
            "AddListener"    -> AddListener <$> v .: "id" <*> v .: "position" <*> (parseURL =<< (v .: "stream_url"))
            "RemoveListener" -> RemoveListener <$> v .: "id"
            "UpdateListener" -> UpdateListener <$> v .: "id" <*> (maybe id (\p l -> l { listenerPosition = p }) <$> v .:? "position")
            "AddSound"       -> AddSound <$> v .: "id" <*> J.parseJSON o
            "AddLocation"    -> AddLocation <$> v .: "id" <*> v .: "position" <*> v .: "radius" <*> v .: "sounds"
            "RemoveLocation" -> RemoveLocation <$> v .: "id"
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

data Options = Options {
    _help :: Bool
  , _socket :: Maybe FilePath
  , _jackPath :: FilePath
  , _jackDriver :: String
  , _maxNumListeners :: Int
  , _monitor :: Bool
  } deriving (Show)

defaultOptions :: Options
defaultOptions = Options {
    _help = False
  , _socket = Nothing
  , _jackPath = "jackd"
  , _jackDriver = "dummy"
  , _monitor = False
  , _maxNumListeners = 1
  }

$( makeLenses [''Options] )

data Environment = Env {
    _options :: Options
  , _temporaryDirectory :: FS.FilePath
  , _jackServerName :: String
  } deriving (Show)

makeEnv :: Options -> FS.FilePath -> String -> Environment
makeEnv = Env

$( makeLenses [''Environment] )

data EventSinks = EventSinks {
    fireAddSound :: (SoundId, Sound) -> IO ()
  , fireAddLocation :: (LocationId, Location) -> IO ()
  , fireRemoveLocation :: LocationId -> IO ()
  , fireUpdateLocation :: (LocationId, Location -> Location) -> IO ()
  , fireAddListener :: (ListenerId, Listener) -> IO ()
  , fireRemoveListener :: ListenerId -> IO ()
  , fireUpdateListener :: (ListenerId, Listener -> Listener) -> IO ()
  , fireQuit :: () -> IO ()
  }

requestToEvent :: EventSinks -> Request -> IO ()
requestToEvent es (AddSound i sound) = fireAddSound es (i, sound)
requestToEvent es (AddLocation i p r ss) = fireAddLocation es (i, Location p r ss)
requestToEvent es (RemoveLocation i) = fireRemoveLocation es i
requestToEvent es (UpdateLocation i f) = fireUpdateLocation es (i, f)
requestToEvent es (AddListener i x1 x2) = fireAddListener es (i, Listener x1 x2)
requestToEvent es (RemoveListener i) = fireRemoveListener es i
requestToEvent es (UpdateListener i f) = fireUpdateListener es (i, f)
requestToEvent es Quit = fireQuit es ()

arguments :: Mode Options
arguments =
    mode "streamero-sound-engine" defaultOptions "Streamero Sound Engine v0.1"
         (flagArg (upd socket . Just) "SOCKET") $
         [ flagHelpSimple (setL help True)
         , flagReq ["jack-path"] (upd jackPath) "STRING" "Path to Jack command"
         , flagReq ["jack-driver", "d"] (upd jackDriver) "STRING" "Jack driver"
         , flagBool ["monitor"] (setL monitor) "Whether to monitor the stream locally"
         , flagReq ["max-num-listeners", "n"] (upd maxNumListeners . read) "NUMBER" "Maximum number of listeners"
         ]
    where
        upd what x = Right . setL what x

withSC :: Environment -> String -> SC.Server a -> IO a
withSC env clientName =
    SC.withSynth
        SC.defaultServerOptions {
            SC.numberOfInputBusChannels = 2
          , SC.numberOfOutputBusChannels = 2 * (maxNumListeners . options ^$ env)
          {-, SC.ugenPluginPath = Just [ "./plugins" ]-}
          }
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

logE :: R.Event t String -> R.NetworkDescription t ()
logE = R.reactimate . fmap logLn

traceE :: Show a => R.Event t a -> R.NetworkDescription t ()
traceE = logE . fmap show

data Command =
    QuitServer
  | Execute (SC.ServerT IO ())

-- | Return SC server loop and command sink.
data ServerHandle = ServerHandle (Command -> IO ()) (MVar.MVar (Maybe SomeException))

newServer :: (SC.ServerT IO () -> IO ()) -> IO ServerHandle
newServer f = do
    toSC <- Chan.newChan
    result <- MVar.newEmptyMVar
    let serverLoop = do
            x <- liftIO $ Chan.readChan toSC
            case x of
                QuitServer -> return ()
                Execute action -> do
                    action
                    serverLoop
        sink = Chan.writeChan toSC
        handle = ServerHandle sink result
    void $ forkIO $ catch (ignoreSignals [keyboardSignal] (f serverLoop) >> MVar.putMVar result Nothing) (MVar.putMVar result . Just)
    return handle

execute_ :: ServerHandle -> SC.ServerT IO () -> IO ()
execute_ (ServerHandle sink _) = sink . Execute

executeWith :: ServerHandle -> (a -> IO ()) -> SC.ServerT IO a -> IO ()
executeWith handle sink action = execute_ handle (action >>= liftIO . sink)

execute :: ServerHandle -> SC.ServerT IO a -> IO a
execute handle action = do
    result <- MVar.newEmptyMVar
    executeWith handle (MVar.putMVar result) action
    MVar.takeMVar result

quit :: ServerHandle -> IO ()
quit (ServerHandle sink _) = sink QuitServer

waitForServer :: ServerHandle -> IO ()
waitForServer (ServerHandle _ result) = do
    x <- MVar.takeMVar result
    case x of
        Nothing -> return ()
        Just e -> throw e

newAsyncEvent :: R.NetworkDescription t (R.Event t a, IO a -> IO ())
newAsyncEvent = do
    (evt, fire) <- R.newEvent
    return (evt, \action -> void $ forkIO $ action >>= fire)

newSyncEvent :: R.NetworkDescription t (R.Event t a, IO a -> IO ())
newSyncEvent = do
    (evt, fire) <- R.newEvent
    return (evt, \action -> action >>= fire)

scanlE :: (a -> b -> a) -> a -> R.Event t b -> R.Event t a
scanlE f a0 = R.accumE a0 . fmap (flip f)

sample :: R.Behavior t a -> R.Event t b -> R.Event t (a, b)
sample = R.apply . fmap (,)

sample_ :: R.Behavior t a -> R.Event t b -> R.Event t a
sample_ = R.apply . fmap const

zipB :: R.Behavior t a -> R.Behavior t b -> R.Behavior t (a, b)
zipB a b = (,) <$> a <*> b

soundFileInfo :: FilePath -> IO SoundFileInfo
soundFileInfo path = do
    info <- SF.getInfo path
    return $ SoundFileInfo (SF.samplerate info) (SF.channels info) (fromIntegral $ SF.frames info)

data Player = Player SC.Buffer SC.Synth deriving Show

data LocationState = LocationState {
    bus :: SC.AudioBus
  , players :: H.HashMap SoundId Player
  } deriving (Show)

type SoundMap = H.HashMap SoundId (Sound, SoundFileInfo)
type LocationMap = H.HashMap LocationId (Location, LocationState)
type ListenerMap = H.HashMap ListenerId (Listener, ListenerState)

class ControlName a where
    controlName :: a -> String

instance ControlName String where
    controlName = id

class ToControlValue a where
    toControlValue :: a -> Double

instance ToControlValue Double where
    toControlValue = id

instance ToControlValue Float where
    toControlValue = realToFrac

instance ToControlValue Int where
    toControlValue = fromIntegral

instance ToControlValue Bool where
    toControlValue True = 1
    toControlValue False = 0

instance ToControlValue SC.AudioBus where
    toControlValue = fromIntegral . SC.busId

instance ToControlValue SC.ControlBus where
    toControlValue = fromIntegral . SC.busId

instance ToControlValue SC.Buffer where
    toControlValue = fromIntegral . SC.bufferId

control :: (ToControlValue a) => String -> a -> (String, Double)
control s a = (s, toControlValue a)

playerSynthDef :: Int -> SC.Loop -> SC.UGen
playerSynthDef nc loop = SC.out (SC.control SC.KR "out" 0) $ SC.mix $ SC.diskIn nc (SC.control SC.IR "buffer" (-1)) loop

mkPlayerSynthDef :: Int -> SC.Loop -> SC.ServerT IO SC.SynthDef
mkPlayerSynthDef nc = SC.exec' immediately . SC.async . SC.d_recv ("player-" ++ show nc) . playerSynthDef nc

truncatePowerOfTwo :: (Bits.Bits a, Integral a) => a -> a
truncatePowerOfTwo = Bits.shiftL 1 . truncate . logBase (2::Double) . fromIntegral

-- FIXME: Use playBuf for small sound files (e.g. <= 32768)
diskBufferSize :: SoundFileInfo -> Int
diskBufferSize = fromIntegral . min 32768 . truncatePowerOfTwo . numFrames 

newPlayer :: (Int -> SC.Loop -> SC.ServerT IO SC.SynthDef) -> SC.AudioBus -> Sound -> SoundFileInfo -> SC.ServerT IO Player
newPlayer mkSynthDef bus sound info = do
    let nc = numChannels info
    sd <- mkSynthDef nc SC.Loop
    g <- SC.rootNode
    {-SC.exec' immediately $ SC.dumpOSC SC.TextPrinter-}
    (buffer, synth) <- SC.exec' immediately $
        SC.b_alloc (diskBufferSize info) nc `SC.whenDone` \buffer ->
            SC.b_read buffer (path sound) Nothing Nothing Nothing True `SC.whenDone` \_ -> do
                synth <- SC.s_new sd SC.AddToTail g
                            [ control "buffer" buffer
                            , control "out" bus ]
                SC.resource (buffer, synth)
    return $ Player buffer synth

newLocation :: SoundMap -> (LocationId, Location) -> SC.ServerT IO (LocationId, (Location, LocationState))
newLocation soundMap (locationId, location) = do
    bus <- SC.newAudioBus 1
    players <- mapM (uncurry (newPlayer mkPlayerSynthDef bus) . (H.!) soundMap) (locationSounds location)
    return (locationId, (location, LocationState bus (H.fromList (zip (locationSounds location) players))))

data ListenerState = ListenerState {
    outputBus :: SC.AudioBus
  , patchCables :: H.HashMap LocationId SC.Synth
  , streamer :: ProcessHandle
  }

patchCableSynthDef :: SC.UGen
patchCableSynthDef =
    let i = SC.in' 1 SC.AR (SC.control SC.KR "in" 0)
        o = SC.out (SC.control SC.KR "out" 0)
    in o (i * SC.lag (SC.control SC.KR "level" 0) 0.1)

-- TODO: Use control-monad-exception for exception reporting
--       There also needs to be a way to communicate exceptions back from
--       the SC engine loop to the reactive event network.
findOutputBus :: Environment -> ListenerMap -> SC.ServerT IO SC.AudioBus
findOutputBus env listeners = do
    case avail List.\\ used of
        [] -> failure SC.NoFreeIds
        (i:_) -> SC.outputBus 2 i
    where used = map (fromIntegral.SC.busId.outputBus.snd) . H.elems $ listeners
          avail = map (*2) [0..getL (maxNumListeners.options) env - 1]

startDarkice :: Environment -> String -> URL -> IO ProcessHandle
startDarkice env name url = do
    FS.writeTextFile darkiceCfgFile $ Darkice.configText darkiceCfg
    SProc.createProcess (logStrLn $ "STREAMER:" ++ name) (Darkice.createProcess "darkice" darkiceOpts)
    where darkiceCfgFile = FS.append (temporaryDirectory ^$ env) (FS.decodeString $ name ++ ".cfg")
          darkiceCfg = Darkice.defaultConfig {
                          Darkice.jackClientName = Just name
                        , Darkice.server = case URL.url_type url of
                                            URL.Absolute host -> URL.host host
                                            _ -> Darkice.server Darkice.defaultConfig
                        , Darkice.port = case URL.url_type url of
                                            URL.Absolute host -> maybe (Darkice.port Darkice.defaultConfig) fromIntegral (URL.port host)
                                            _ -> Darkice.port Darkice.defaultConfig
                        , Darkice.password = "h3aRh3aR"
                        , Darkice.mountPoint = URL.url_path url
                        , Darkice.bufferSize = 1 }
          darkiceOpts = Darkice.defaultOptions {
                          Darkice.configFile = Just darkiceCfgFile }

makeConnectionMap :: Bool -> Environment -> SJack.Connections
makeConnectionMap monitor env = concatMap f [0..getL (maxNumListeners.options) env - 1]
    where
        f i = let stream = "stream-" ++ show i
                  stream1 = "left"
                  stream2 = "right"
                  sc = "supercollider"
                  sc1 = "out_" ++ show (i + 1)
                  sc2 = "out_" ++ show (i + 2)
              in [ ((sc,sc1),(stream,stream1))
                 , ((sc,sc2),(stream,stream2)) ]
                 ++ if monitor
                    then [ ((sc,sc1),("system","playback_1"))
                         , ((sc,sc2),("system","playback_2")) ]
                    else []

streamName :: Integral a => a -> String
streamName a = "stream-" ++ show (fromIntegral a :: Int)

distanceScaling :: Double -> Double -> Double -> Double -> Double
distanceScaling rolloffFactor refDist maxDist dist
    | dist > maxDist = 0
    | otherwise      = refDist / (refDist + rolloffFactor * (dist' - refDist))
        where dist' = min (max refDist dist) maxDist

locationDistanceScaling :: Location -> Double -> Double
locationDistanceScaling = distanceScaling 1 1 . radius

newListener :: Environment -> LocationMap -> ListenerMap -> (ListenerId, Listener) -> SC.ServerT IO (ListenerId, (Listener, ListenerState))
newListener env locations listeners (listenerId, listener) = do
    -- TODO: Only send SynthDef once
    sd <- SC.exec' immediately . SC.async $ SC.d_recv "patchCable" patchCableSynthDef
    g <- SC.rootNode
    output <- findOutputBus env listeners
    cables <- T.forM locations $ \(l, s) -> do
        let dist = distance (listenerPosition listener) (position l)
            level = locationDistanceScaling l dist
        SC.exec immediately $ SC.s_new sd SC.AddToTail g [ control "in" (bus s)
                                                         , control "out" output
                                                         , control "level" level ]
    darkice <- liftIO $ startDarkice env (streamName . SC.busId $ output) (streamURL listener)
    return (listenerId, (listener, ListenerState output cables darkice))

updateListener :: LocationMap -> Listener -> ListenerState -> SC.ServerT IO ()
updateListener locations listener state = do
    let cables = patchCables state
    F.forM_ (H.keys cables) $ \lid -> do
        let l = fst (locations H.! lid)
            dist = distance (listenerPosition listener) (position l)
            level = locationDistanceScaling l dist
        liftIO $ logLn $ "Distance: " ++ show (lid, dist, level)
        SC.exec immediately $ SC.n_set (cables H.! lid) [ control "level" level ]

freeListener :: ListenerState -> SC.ServerT IO ()
freeListener state = do
    -- Free synths
    SC.exec immediately $ F.forM_ (patchCables state) SC.n_free
    -- Free output bus
    SC.freeBus $ outputBus state
    -- Stop stream
    liftIO $ do
        SProc.interruptProcess (streamer state)
        logLn $ "Stopped stream"

freeListeners :: ListenerMap -> SC.ServerT IO ()
freeListeners = F.mapM_ (freeListener.snd)

type Time = Double

newTimer :: R.NetworkDescription t (Time, R.Event t Time, Time -> IO ())
newTimer = do
    chan <- liftIO STM.newTChanIO
    liftIO $ void $ forkIO $ threadLoop chan
    (evt, fire) <- R.newEvent
    t0 <- liftIO OSC.utcr
    return (t0, evt, \t -> STM.atomically $ STM.writeTChan chan (t, fire))
    where
        threadLoop chan = do
            (t, fire) <- STM.atomically $ STM.readTChan chan
            OSC.pauseThreadUntil t
            fire t
            threadLoop chan

conduitIO :: MonadIO m => (input -> IO ()) -> (IO output) -> C.Conduit input m output
conduitIO inputFunc outputFunc = do
    x <- C.await
    case x of
        Nothing -> return ()
        Just input -> do
            liftIO $ inputFunc input
            output <- liftIO outputFunc
            C.yield output
            conduitIO inputFunc outputFunc

execute' :: (SC.ServerT IO () -> IO ()) -> (a -> IO ()) -> SC.ServerT IO a -> IO ()
execute' f g m = f (m >>= liftIO . g)

main :: IO ()
main = do
    opts <- processArgs arguments
    if help ^$ opts
        then print $ helpText [] HelpFormatDefault arguments
        else do
            withTemporaryDirectory "streamero_" $ \tmpDir_ -> do
                let tmpDir = FS.decodeString tmpDir_
                    jackOptions = (SJack.defaultOptions (FS.encodeString $ FS.basename tmpDir)) {
                                        SJack.driver = jackDriver ^$ opts
                                      , SJack.timeout = Just 5000
                                      , SJack.temporary = False
                                      }
                logLn $ "Starting with temporary directory " ++ tmpDir_
                SJack.withJack (logStrLn "JACK") (jackPath ^$ opts) jackOptions $ \serverName -> do
                    logLn $ "Jack server name: " ++ serverName
                    let env = makeEnv opts tmpDir serverName
                        connectionMap = makeConnectionMap (monitor ^$ opts) env
                        run source sink = do
                            engine <- newServer $ withSC env "supercollider"
                            {-quitVar <- MVar.newEmptyMVar-}
                            fromEngineVar <- MVar.newEmptyMVar
                            eventSinks <- MVar.newEmptyMVar
                            let --toEngine = executeE engine
                                fromEngine = MVar.takeMVar fromEngineVar
                                send = MVar.putMVar fromEngineVar
                                {-quitEngine = MVar.putMVar quitVar ()-}
                                {-quitEngine = quit engine-}
                                missing cmd = send $ Error $ "API call " ++ cmd ++ " not yet implemented"
                            {-withSC env "supercollider" $ do-}
                                {-toEngine <- SC.capture-}
                            let networkDescription :: forall t . R.NetworkDescription t ()
                                networkDescription = do
                                -- Request events
                                (eAddSound, fAddSound) <- R.newEvent
                                (eAddLocation, fAddLocation) <- R.newEvent
                                (eRemoveLocation, fRemoveLocation) <- R.newEvent
                                (eUpdateLocation, fUpdateLocation) <- R.newEvent
                                (eAddListener, fAddListener) <- R.newEvent
                                (eRemoveListener, fRemoveListener) <- R.newEvent
                                (eUpdateListener, fUpdateListener) <- R.newEvent
                                (eQuit, fQuit) <- R.newEvent
                                liftIO $ MVar.putMVar eventSinks $ EventSinks fAddSound fAddLocation fRemoveLocation fUpdateLocation fAddListener fRemoveListener fUpdateListener fQuit

                                -- Read sound file info (a)synchronously
                                {-(eSoundFileInfo, fSoundFileInfo) <- newAsyncEvent-}
                                (eSoundFileInfo, fSoundFileInfo) <- newSyncEvent
                                -- FIXME: Catch exception and report error when file not found.
                                R.reactimate $ (\(i, s) -> fSoundFileInfo $ do { si <- soundFileInfo (path s) ; return (i, (s, si)) }) <$> eAddSound
                                traceE eAddSound
                                -- Update sound map
                                let eSounds = scanlE (flip (uncurry H.insert)) H.empty eSoundFileInfo
                                    bSounds = R.stepper H.empty eSounds
                                -- Return status
                                R.reactimate $ send Ok <$ eSounds
                                R.reactimate $ print <$> eSounds
                                {-R.reactimate $ print <$> eSoundFileInfo-}
                                {-R.reactimate $ print <$> eAddSound-}

                                -- AddLocation
                                (eLocationState, fLocationState) <- R.newEvent
                                {-R.reactimate $ print <$> sample bSounds eLocations-}
                                R.reactimate $ executeWith engine fLocationState <$> (uncurry newLocation <$> sample bSounds eAddLocation)
                                R.reactimate $ print <$> eLocationState
                                let eLocations = scanlE (flip (uncurry H.insert)) H.empty eLocationState
                                    bLocations = R.stepper H.empty eLocations
                                R.reactimate $ print <$> eLocations
                                R.reactimate $ send Ok <$ eLocations

                                -- RemoveLocation
                                R.reactimate $ missing "RemoveLocation" <$ eRemoveLocation

                                -- UpdateLocation
                                R.reactimate $ missing "UpdateLocation" <$ eUpdateLocation

                                -- AddListener
                                (eListenerState, fListenerState) <- R.newEvent
                                let eUpdatedListener = R.apply ((\h (k, f) -> let (l, s) = h H.! k in (k, (f l, s))) <$> bListeners) eUpdateListener
                                    eListeners = R.accumE H.empty $ (uncurry H.insert <$> eListenerState)
                                                                    `R.union`
                                                                    (H.delete <$> eRemoveListener)
                                                                    `R.union`
                                                                    ((\(k, (l, _)) -> H.adjust (first (const l)) k) <$> eUpdatedListener)
                                    bListeners = R.stepper H.empty eListeners
                                R.reactimate $ executeWith engine fListenerState <$> ((uncurry . uncurry $ newListener env) <$> sample (bLocations `zipB` bListeners) eAddListener)
                                R.reactimate $ executeWith engine (const (return ())) <$> ((\(lm, (l, s)) -> updateListener lm l s) <$> sample bLocations (snd <$> eUpdatedListener))
                                R.reactimate $ send Ok <$ eListeners
                                R.reactimate $ logLn . URL.exportURL . streamURL . snd <$> eAddListener

                                -- Quit
                                (eFreedListeners, fFreedListeners) <- R.newEvent
                                R.reactimate $ executeWith engine fFreedListeners <$> (freeListeners <$> sample_ bListeners eQuit)
                                R.reactimate $ (quit engine >> send Ok) <$ eFreedListeners
                            -- Compile network and get event sinks
                            network <- R.compile networkDescription
                            sinks <- MVar.takeMVar eventSinks
                            -- Set up interrupt handler
                            catchSignals (fireQuit sinks ()) [keyboardSignal]
                            -- Activate network
                            R.actuate network
                            -- Sync with engine
                            {-execute engine $ SC.exec' immediately $ SC.dumpOSC SC.TextPrinter-}
                            execute engine $ return ()
                            -- Set up connections to and from JSON I/O network
                            void $ forkIO $ source =$= conduitIO (requestToEvent sinks) fromEngine $$ sink
                            {-MVar.takeMVar quitVar-}
                            waitForServer engine
                            -- Deactivate network
                            R.pause network
                    liftIO $ do
                        let delay = 1
                        logLn $ "Waiting " ++ show delay ++ " seconds before trying to connect to Jack"
                        OSC.pauseThread delay
                    void $ SJack.withPatchBay serverName "patchbay" connectionMap $ \client -> do
                        lift $ case socket ^$ opts of
                            {-Nothing ->-}
                                {--- Readline interface-}
                                {-let source = sourceReadline "> " =$= CL.map BS8.pack =$= parseJsonStream =$= fromJson-}
                                    {-sink = toJson =$= unlines =$= CB.sinkHandle stdout-}
                                {-in run source sink-}
                            Nothing ->
                                -- Terminal interface
                                let source = CB.sourceHandle stdin =$= C.mapOutput (BC.fromChunks . (:[])) CB.lines =$= parseJsonMessage =$= {- printC =$= -} fromJson
                                    sink = toJson =$= unlines =$= CB.sinkHandle stdout
                                in run source sink
                            Just socketFile ->
                                -- Socket interface
                                withUnixSocket socketFile $ \s ->
                                    let source = C.sourceSocket s =$= message =$= parseJsonMessage =$= {- printC =$= -} fromJson
                                        sink = toJson =$= unmessage =$= C.sinkSocket s
                                    in run source sink
                logLn "done."
