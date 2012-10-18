{-# LANGUAGE ExistentialQuantification
           , FlexibleInstances
           , GeneralizedNewtypeDeriving
           , OverloadedStrings
           -- , TemplateHaskell
           , TypeSynonymInstances #-}
import           Control.Applicative
import           Control.Arrow (first, second)
import           Control.Category ((.))
import           Control.Concurrent (forkIO, myThreadId, threadDelay)
import qualified Control.Concurrent.Chan as Chan
import qualified Control.Concurrent.MVar as MVar
import qualified Control.Concurrent.STM as STM
import           Control.Exception.Lifted as E
import           Control.Failure (Failure, failure)
import           Control.Monad (foldM, join, mzero, unless, void, when)
import           Control.Monad.IO.Class (MonadIO, liftIO)
import           Control.Monad.Trans.Class (lift)
import           Data.Aeson (FromJSON(..), ToJSON(..), Value(..), (.=), (.:), (.:?), (.!=), object)
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
import qualified Data.Hashable as H
import qualified Data.HashMap.Strict as H
import           Data.Lens.Common
--import           Data.Lens.Template
import qualified Data.List as List
import           Data.Maybe (catMaybes)
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
import           Prelude hiding ((.), catch, unlines)
import           Reactive.Banana ((<@>), (<@))
import qualified Reactive.Banana as R
import           Sound.OpenSoundControl (immediately)
import qualified Sound.OpenSoundControl as OSC
import qualified Sound.SC3.UGen as SC
import qualified Sound.SC3.Server.Allocator as SC
import qualified Sound.SC3.Server.Monad as SC
import qualified Sound.SC3.Server.Monad.Command as SC
import qualified Sound.SC3.Server.Monad.Process as SC
import qualified Sound.SC3.Server.Monad.Request as SC
import qualified Sound.File.Sndfile as SF
import qualified Streamero.Darkice as Darkice
import qualified Streamero.Jack as SJack
import qualified Streamero.Process as SProc
{-import           Streamero.Readline (sourceReadline)-}
import           Streamero.SC3 (toStereo)
import           Streamero.Signals (ignoreSignals, catchSignals)
import qualified Streamero.SoundFile as SF
import           System.Console.CmdArgs.Explicit
import qualified System.Directory as Dir
import           System.FilePath ((</>))
import           System.IO
import           System.Posix.Signals (Signal, sigINT, sigTERM)
import           System.Process (ProcessHandle)
import           System.Unix.Directory (withTemporaryDirectory)

-- TODO:
-- * store id in listener, location and sound for easier access when traversing hash maps
-- * store listener and location state along with data in hash map as before (now that patch cables are separate)
-- * Use individual events for field updates
-- * Refactor into modules
-- * Place player synths in one group and patch cables in another (ordered after the first group) to avoid order of execution problems
-- * Only update the patch cables for locations the listener is currently in.
-- * Use playBuf for small sound files (e.g. <= 32768)

catMaybesC :: Monad m => C.Conduit (Maybe a) m a
catMaybesC = do
    v <- C.await
    case v of
        Nothing -> return ()
        Just Nothing -> catMaybesC
        Just (Just a) -> C.yield a >> catMaybesC

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
parseJsonStream = C.sequence (C.sinkParser jsonOrWhite) =$= catMaybesC

data Coord = Coord { latitude :: Double, longitude :: Double }
             deriving (Eq, Show)

instance FromJSON Coord where
    parseJSON (Object v) = Coord <$> v .: "latitude" <*> v .: "longitude"
    parseJSON _          = mzero

-- | Add one coordinate as an offset to another.
offset :: Coord -> Coord -> Coord
offset c1 c2 = Coord (latitude c1 + latitude c2) (longitude c1 + longitude c2)

-- | Distance in meters between two coordinates.
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

newtype SoundId = SoundId Int deriving (Eq, Show, H.Hashable, FromJSON)

data Sound =
    SoundFile {
        path :: FilePath
      , loop :: Bool
      , isEvent :: Bool
    }
    deriving (Show)

instance FromJSON Sound where
    parseJSON (Object v) = do
        t <- v .: "type" :: J.Parser Text
        case t of
            "sample" -> SoundFile <$> v .: "path" <*> v .: "loop" <*> v .:? "event" .!= False
            _        -> mzero
    parseJSON _          = mzero

data SoundFileInfo = SoundFileInfo {
    sampleRate :: Int
  , numChannels :: Int
  , numFrames :: Word64
  } deriving (Show)

newtype ListenerId = ListenerId Text deriving (Eq, Show, H.Hashable, FromJSON)

data Listener = Listener {
    listenerPosition :: Coord
  , streamURL :: URL
  } deriving (Show)

data Reference = Absolute | Relative deriving (Eq, Show)

newtype LocationId = LocationId Int deriving (Eq, Show, H.Hashable, FromJSON)

data Location = Location {
  position :: Coord
, reference :: Reference
, radius :: Double
, locationSounds :: [SoundId]
} deriving (Show)

instance FromJSON Location where
    parseJSON (Object v) = Location <$>
                            v .: "position" <*>
                            ((\b -> if b then Relative else Absolute) <$> v .:? "relative" .!= False) <*>
                            v .: "radius" <*>
                            v .:? "sounds" .!= []
    parseJSON _ = mzero

locationDistance :: Location -> Coord -> Double
locationDistance location coord =
    case reference location of
        Absolute -> distance coord (position location)
        Relative -> distance coord (coord `offset` position location)

data LocationState = LocationState {
  bus :: SC.AudioBus
, players :: H.HashMap SoundId Player
} deriving (Show)

data Request =
    Quit
  | AddListener ListenerId Coord URL
  | RemoveListener ListenerId
  | UpdateListener ListenerId (Listener -> Listener)
  | AddSound SoundId Sound
  | AddLocation LocationId Location
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
            "AddLocation"    -> AddLocation <$> v .: "id" <*> J.parseJSON o
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
  , _scUdpPort :: Int
  , _monitor :: Bool
  , _local :: Bool
  } deriving (Show)

defaultOptions :: Options
defaultOptions = Options {
    _help = False
  , _socket = Nothing
  , _jackPath = "jackd"
  , _jackDriver = "dummy"
  , _maxNumListeners = 1
  , _scUdpPort = 57110
  , _monitor = False
  , _local = False
  }

-- $( makeLenses [''Options] )

help = lens
  _help (\x s -> s {
  _help = x })
socket = lens
  _socket (\x s -> s {
  _socket = x })
jackPath = lens
  _jackPath (\x s -> s {
  _jackPath = x })
jackDriver = lens
  _jackDriver (\x s -> s {
  _jackDriver  = x })
maxNumListeners = lens
  _maxNumListeners (\x s -> s {
  _maxNumListeners = x })
scUdpPort = lens
  _scUdpPort (\x s -> s {
  _scUdpPort = x })
monitor = lens
  _monitor (\x s -> s {
  _monitor = x })
local = lens
  _local (\x s -> s {
  _local = x })

data Environment = Env {
    _options :: Options
  , _temporaryDirectory :: FS.FilePath
  , _jackServerName :: String
  } deriving (Show)

makeEnv :: Options -> FS.FilePath -> String -> Environment
makeEnv = Env

-- $( makeLenses [''Environment] )
options = lens
  _options (\x s -> s {
  _options = x })
temporaryDirectory = lens
  _temporaryDirectory (\x s -> s {
  _temporaryDirectory = x })
jackServerName = lens
  _jackServerName (\x s -> s {
  _jackServerName = x })

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
requestToEvent es (AddLocation i l) = fireAddLocation es (i, l)
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
         , flagReq ["udp-port"] (upd scUdpPort . read) "INTEGER" "SuperCollider UDP port"
         , flagBool ["monitor"] (setL monitor) "Whether to monitor the stream locally"
         , flagReq ["max-num-listeners", "n"] (upd maxNumListeners . read) "INTEGER" "Maximum number of listeners"
         , flagBool ["local"] (setL local) "Stream to a local instance of icecast"
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
            SC.networkPort = SC.UDPPort (scUdpPort . options ^$ env)
          , SC.hardwareDeviceName = Just $ (jackServerName ^$ env) ++ ":" ++ clientName
          }
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

logE :: String -> R.Event t String -> R.NetworkDescription t ()
logE s = R.reactimate . fmap (logStrLn s)

traceE :: Show a => String -> R.Event t a -> R.NetworkDescription t ()
traceE s = logE s . fmap show

data Command =
    QuitServer
  | Execute (SC.ServerT IO ())

-- | Return SC server loop and command sink.
data ServerHandle = ServerHandle (Command -> IO ()) (MVar.MVar (Maybe SomeException))

newServer :: [Signal] -> (SC.ServerT IO () -> IO ()) -> IO ServerHandle
newServer blockedSignals f = do
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
    void $ forkIO $ catch (ignoreSignals blockedSignals (f serverLoop) >> MVar.putMVar result Nothing) (MVar.putMVar result . Just)
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

newEngineEvent :: ServerHandle -> R.Event t (SC.ServerT IO a) -> R.NetworkDescription t (R.Event t a)
newEngineEvent handle evt = do
    (evt', fire) <- R.newEvent
    R.reactimate $ executeWith handle fire <$> evt
    return evt'

scanlE :: (a -> b -> a) -> a -> R.Event t b -> R.Event t a
scanlE f a0 = R.accumE a0 . fmap (flip f)

sample :: R.Behavior t a -> R.Event t b -> R.Event t (a, b)
sample = R.apply . fmap (,)

sample_ :: R.Behavior t a -> R.Event t b -> R.Event t a
sample_ = (<@)

zipB :: R.Behavior t a -> R.Behavior t b -> R.Behavior t (a, b)
zipB a b = (,) <$> a <*> b

lookupB :: (Eq k, H.Hashable k) => R.Behavior t (H.HashMap k a) -> R.Event t k -> R.Event t (k, a)
lookupB h = R.filterJust . R.apply ((\m k -> fmap ((,)k) (H.lookup k m)) <$> h)

lookupB_ :: (Eq k, H.Hashable k) => R.Behavior t (H.HashMap k a) -> R.Event t k -> R.Event t a
lookupB_ h = fmap snd . lookupB h

hashMapB :: (Eq k, H.Hashable k) => R.Event t (k, v) -> R.Event t k -> R.Event t (k, v -> v) -> (R.Event t (H.HashMap k v), R.Behavior t (H.HashMap k v))
hashMapB insert remove update =
  let e = (uncurry H.insert <$> insert)
          `R.union`
          (H.delete <$> remove)
          `R.union`
          (uncurry (flip H.adjust) <$> update)
      dup x = (x, x)
      result = fmap
  in R.mapAccum H.empty (result dup <$> e)

--hashMapStateB :: (Eq k, H.Hashable k) =>
--    ServerHandle
-- -> R.Event t (k, v)
-- -> (k -> v -> SC.ServerT IO s)
-- -> R.Event t k -> (k -> s -> SC.ServerT IO ())
-- -> R.Event t (k, v -> v) -> (k -> v -> s -> SC.ServerT IO s)
-- -> (R.Event t (H.HashMap k (v, s)), R.Behavior t (H.HashMap k (v, s)))

soundFileInfo :: FilePath -> IO SoundFileInfo
soundFileInfo path = do
    info <- SF.getInfo path
    return $ SoundFileInfo (SF.samplerate info) (SF.channels info) (fromIntegral $ SF.frames info)

duration :: SoundFileInfo -> Double
duration i = fromIntegral (numFrames i) / fromIntegral (sampleRate i)

type SoundMap = H.HashMap SoundId (Sound, SoundFileInfo)

lookupSounds :: [SoundId] -> SoundMap -> SoundMap
--lookupSounds ids = flip H.intersection . H.fromList (map ((,)()) ids)
lookupSounds ids soundMap = List.foldl' (\h i -> case H.lookup i soundMap of
                                                    Nothing -> h
                                                    Just s -> H.insert i s h)
                                        H.empty
                                        ids

data Player = Player SC.Buffer SC.Synth deriving Show

type LocationMap = H.HashMap LocationId Location
type LocationStateMap = H.HashMap LocationId LocationState
type ListenerMap = H.HashMap ListenerId Listener
type ListenerStateMap = H.HashMap ListenerId ListenerState

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
playerSynthDef nc loop = SC.out (SC.control SC.KR "out" 0) $ toStereo $ SC.diskIn nc (SC.control SC.IR "buffer" (-1)) loop

mkPlayerSynthDef :: Int -> SC.Loop -> SC.ServerT IO SC.SynthDef
mkPlayerSynthDef nc loop = SC.exec' immediately . SC.async . SC.d_recv ("player-" ++ show loop ++ "-" ++ show nc) . playerSynthDef nc $ loop

truncatePowerOfTwo :: (Bits.Bits a, Integral a) => a -> a
truncatePowerOfTwo = Bits.shiftL 1 . truncate . logBase (2::Double) . fromIntegral

unsupportedFormat :: SF.Exception -> Maybe SF.Exception
unsupportedFormat e =
    case SF.errorString e of
        "File contains data in an unknown format." -> Just e
        _ -> Nothing

addSound i s = do
    (s', si) <- E.catchJust unsupportedFormat (((,)s) `fmap` soundFileInfo (path s)) $ \e -> do
        let cacheDir = "tmp/cache/streamero-sound-engine"
            cacheFile = cacheDir </> show i ++ ".flac"
        cacheFileExists <- Dir.doesFileExist cacheFile
        unless cacheFileExists $ do
            Dir.createDirectoryIfMissing True cacheDir
            SF.toFLAC (path s) cacheFile
        ((,) s { path = cacheFile }) `fmap` soundFileInfo cacheFile
    return (i, (s', si))

diskBufferSize :: SoundFileInfo -> Int
diskBufferSize = fromIntegral . min 32768 . truncatePowerOfTwo . numFrames 

newPlayer :: (Int -> SC.Loop -> SC.ServerT IO SC.SynthDef) -> SC.AudioBus -> Sound -> SoundFileInfo -> SC.ServerT IO Player
newPlayer mkSynthDef bus sound info = do
    let nc = numChannels info
    sd <- mkSynthDef nc (if isEvent sound then SC.NoLoop else SC.Loop)
    g <- SC.rootNode
    {-SC.exec' immediately $ SC.dumpOSC SC.TextPrinter-}
    (buffer, synth) <- SC.exec' immediately $
        SC.b_alloc (diskBufferSize info) nc `SC.whenDone` \buffer ->
            SC.b_read buffer (path sound) Nothing Nothing Nothing True `SC.whenDone` \_ -> do
                -- FIXME: Put all player synths in one group, see TODO above
                synth <- SC.s_new sd SC.AddToHead g
                            [ control "buffer" buffer
                            , control "out" bus ]
                SC.resource (buffer, synth)
    return $ Player buffer synth

freePlayer :: (Functor m, MonadIO m) => Player -> SC.RequestT m ()
freePlayer (Player buffer synth) = SC.n_free synth >> void (SC.async (SC.b_free buffer))

-- | Play an event sound at a certain location.
playSound :: (Listener, ListenerState) -> (Sound, SoundFileInfo) -> SC.ServerT IO Player
playSound (listener, listenerState) (sound, soundFileInfo) = do
    player <- newPlayer mkPlayerSynthDef (outputBus listenerState) sound soundFileInfo
    void $ SC.fork $ do
        liftIO $ OSC.pauseThread (duration soundFileInfo)
        SC.exec immediately $ do
            let Player buffer synth = player
            SC.n_free synth
            SC.async $ SC.b_free buffer
            return ()
        {-liftIO $ logLn "Event player freed"-}
    return player

newLocation :: SoundMap -> LocationId -> Location -> SC.ServerT IO (LocationId, (Location, LocationState))
newLocation soundMap locationId location = do
    bus <- SC.newAudioBus 2
    let sounds = H.filter (not.isEvent.fst) (lookupSounds (locationSounds location) soundMap)
    players <- T.mapM (uncurry (newPlayer mkPlayerSynthDef bus)) sounds
    return (locationId, (location, LocationState bus players))

freeLocation :: LocationId -> LocationState -> SC.ServerT IO LocationId
freeLocation locationId state = do
    -- Free synths
    SC.exec immediately $ F.mapM_ freePlayer (players state)
    -- Free output bus
    SC.freeBus $ bus state
    return locationId

updateLocation :: SoundMap -> LocationId -> Location -> LocationState -> SC.ServerT IO (LocationId, LocationState -> LocationState)
updateLocation soundMap locationId location state = do
    -- Find ids of continuous sounds not yet playing
    let locationSoundMap = H.fromList (map (flip(,)()) (locationSounds location))
        continuous = H.filterWithKey (\k _ -> maybe False (not.isEvent.fst) (H.lookup k soundMap))
        continuousSounds = continuous . flip lookupSounds soundMap
        diff a b = H.keys (H.difference a b)
        soundsToPlay = lookupSounds (H.keys $ continuous (H.difference locationSoundMap (players state))) soundMap
        playersToStop = continuous (H.difference (players state) locationSoundMap)
    -- Start new players
    newPlayers <- T.mapM (uncurry (newPlayer mkPlayerSynthDef (bus state))) soundsToPlay
    -- Stop old players
    SC.exec immediately $ F.mapM_ freePlayer playersToStop
    --liftIO $ logStrLn "updateLocation" $ show (locationSounds location, H.keys soundsToPlay, H.keys playersToStop, newPlayers)
    return (locationId, \s -> s { players = H.union newPlayers (H.filterWithKey (\k _ -> not (H.member k playersToStop)) (players s)) })

-- Gain is 1 for dist < refDist and 0 for dist > maxDist.
-- This is an extension of the OpenAL "Inverse Distance Clamped Model" (equivalent to the )
distanceScaling :: Double -> Double -> Double -> Double -> Double
distanceScaling rolloffFactor refDist maxDist dist
    | dist > maxDist = 0
    | otherwise      = refDist / (refDist + rolloffFactor * (dist' - refDist))
        where dist' = min (max refDist dist) maxDist

locationDistanceScaling :: Location -> Double -> Double
locationDistanceScaling = distanceScaling 0.1 1 . radius
--locationDistanceScaling _ _ = 1.0

data ListenerState = ListenerState {
    outputBus :: SC.AudioBus
  , streamer :: ProcessHandle
  }

type PatchCables = H.HashMap ListenerId (H.HashMap LocationId SC.Synth)

patchCableSynthDef :: SC.UGen
patchCableSynthDef =
    let i = SC.in' 2 SC.AR (SC.control SC.KR "in" 0)
        o = SC.out (SC.control SC.KR "out" 0)
    in o (i * SC.lag (SC.control SC.KR "level" 0) 0.1)

newPatchCable :: SC.Group -> SC.AudioBus -> SC.AudioBus -> Listener -> Location -> SC.ServerT IO SC.Synth
newPatchCable group input output listener location = do
    -- TODO: Only send SynthDef once
    sd <- SC.exec' immediately . SC.async $ SC.d_recv "patchCable" patchCableSynthDef
    let dist = location `listenerDistance` listener
        level = location `locationDistanceScaling` dist
    SC.exec immediately $ SC.s_new sd SC.AddToTail group
                            [ control "in" input
                            , control "out" output
                            , control "level" level ]

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
                  sc1 = "out_" ++ show (2*i + 1)
                  sc2 = "out_" ++ show (2*i + 2)
              in [ ((sc,sc1),(stream,stream1))
                 , ((sc,sc2),(stream,stream2)) ]
                 ++ if monitor
                    then [ ((sc,sc1),("system","playback_1"))
                         , ((sc,sc2),("system","playback_2")) ]
                    else []

streamName :: Integral a => a -> String
streamName a = "stream-" ++ show (fromIntegral a :: Int)

listenerDistance :: Location -> Listener -> Double
listenerDistance location = locationDistance location . listenerPosition

listenerLocations :: Listener -> H.HashMap LocationId Location -> H.HashMap LocationId Location
listenerLocations listener = H.filter (\location -> location `listenerDistance` listener <= radius location)

-- TODO: Use control-monad-exception for exception reporting
--       There also needs to be a way to communicate exceptions back from
--       the SC engine loop to the reactive event network.
findOutputBus :: Environment -> ListenerStateMap -> SC.ServerT IO SC.AudioBus
findOutputBus env listenerStates = do
    case avail List.\\ used of
        [] -> failure SC.NoFreeIds
        (i:_) -> SC.outputBus 2 i
    where used = map (fromIntegral.SC.busId.outputBus) . H.elems $ listenerStates
          avail = map (*2) [0..getL (maxNumListeners.options) env - 1]

newListener :: Environment -> LocationMap -> LocationStateMap -> ListenerStateMap -> ListenerId -> Listener -> SC.ServerT IO (ListenerId, (Listener, ListenerState))
newListener env locations locationStates listenerStates listenerId listener = do
    output <- findOutputBus env listenerStates
    darkice <- liftIO $ startDarkice env (streamName . SC.busId $ output) (streamURL listener)
    return (listenerId, (listener, ListenerState output darkice))

freeListener :: ListenerId -> ListenerState -> SC.ServerT IO ListenerId
freeListener listenerId state = do
    -- Free output bus
    SC.freeBus $ outputBus state
    -- Stop stream
    liftIO $ do
        SProc.interruptProcess (streamer state)
        logLn $ "Stopped stream"
    return listenerId

updatePatchCablesForListener :: LocationMap -> ListenerMap -> PatchCables -> ListenerId -> SC.ServerT IO (PatchCables -> PatchCables)
updatePatchCablesForListener locations listeners patchCables listenerId = do
  case H.lookup listenerId patchCables of
    Nothing -> return id
    Just listenerPatchCables -> do
      F.forM_ (H.keys listenerPatchCables) $ \locationId ->
        case H.lookup locationId listenerPatchCables of
          Nothing -> return ()
          Just synth -> do
            let listener = listeners H.! listenerId
                location = locations H.! locationId
                dist     = location `listenerDistance` listener
                level    = location `locationDistanceScaling` dist
            liftIO $ logStrLn "updatePatchCablesForListener" $ "Distance: " ++ show (listenerPosition listener) ++ " " ++ show (listenerId, dist, level)
            SC.exec immediately $ SC.n_set synth [ control "level" level ]
      return id

updatePatchCablesForLocation :: LocationMap -> ListenerMap -> PatchCables -> LocationId -> SC.ServerT IO (PatchCables -> PatchCables)
updatePatchCablesForLocation locations listeners patchCables locationId = do
  F.forM_ (H.keys patchCables) $ \listenerId ->
    case H.lookup locationId (patchCables H.! listenerId) of
      Just synth -> do
        let listener = listeners H.! listenerId
            location = locations H.! locationId
            dist     = location `listenerDistance` listener
            level    = location `locationDistanceScaling` dist
        liftIO $ logStrLn "updatePatchCablesForLocation" $ "Distance: " ++ show (listenerPosition listener) ++ " " ++ show (listenerId, dist, level)
        SC.exec immediately $ SC.n_set synth [ control "level" level ]
  return id

data LocationEvent = Enter ListenerId LocationId | Leave ListenerId LocationId deriving Show

data EventPlayer = EventPlayer SC.AudioBus SC.Synth (H.HashMap SoundId Player)
type EventPlayerMap = H.HashMap ListenerId (H.HashMap LocationId EventPlayer)

updateEventPlayers :: SoundMap -> LocationMap -> ListenerMap -> ListenerStateMap -> EventPlayerMap -> LocationEvent -> SC.ServerT IO EventPlayerMap
updateEventPlayers sounds locations listeners listenerStates eventPlayers event =
    case event of
        Enter listenerId locationId -> do
            let location = locations H.! locationId
                listener = listeners H.! listenerId
                listenerState = listenerStates H.! listenerId
                eventSounds = H.filter (isEvent.fst) (lookupSounds (locationSounds location) sounds)
                level = locationDistanceScaling location (location `listenerDistance` listener)
            liftIO $ logLn $ show eventSounds
            if H.null eventSounds
                then return eventPlayers
                else do
                    bus <- SC.newAudioBus 2
                    players <- T.mapM (uncurry (newPlayer mkPlayerSynthDef bus)) eventSounds
                    g <- SC.rootNode
                    cable <- newPatchCable g bus (outputBus listenerState) listener location
                    return $! H.insert listenerId
                                       (H.insert locationId
                                                 (EventPlayer bus cable players)
                                                 (H.lookupDefault H.empty listenerId eventPlayers))
                                        eventPlayers
        Leave listenerId locationId ->
            case H.lookup listenerId eventPlayers of
                Nothing -> return eventPlayers
                Just h -> case H.lookup locationId h of
                            Nothing -> return H.empty
                            Just (EventPlayer bus cable players) -> do
                                SC.exec immediately $ do
                                    F.mapM_ freePlayer players
                                    SC.n_free cable
                                SC.freeBus bus
                                return $! H.insert listenerId (H.delete locationId h) eventPlayers

updateEventPlayersForListener :: LocationMap -> ListenerMap -> EventPlayerMap -> ListenerId -> SC.ServerT IO EventPlayerMap
updateEventPlayersForListener locations listeners eventPlayers listenerId = do
    case H.lookup listenerId eventPlayers of
        Nothing -> return ()
        Just es -> F.forM_ (H.keys es) $ \locationId -> do
            let listener = listeners H.! listenerId
                location = locations H.! locationId
                dist = location `listenerDistance` listener
                level = location `locationDistanceScaling` dist
                EventPlayer _ cable _ = es H.! locationId
            SC.exec immediately $ SC.n_set cable [ control "level" level ]            
    return eventPlayers

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
    hSetBuffering stdout LineBuffering
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
                        handledSignals = [sigINT, sigTERM]
                        run source sink = do
                            engine <- newServer handledSignals $ withSC env "supercollider"
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
                                liftIO $ MVar.putMVar eventSinks
                                       $ EventSinks fAddSound
                                                    fAddLocation
                                                    fRemoveLocation
                                                    fUpdateLocation
                                                    fAddListener
                                                    fRemoveListener
                                                    fUpdateListener
                                                    fQuit

                                -- Debugging
                                traceE "eAddSound" eAddSound
                                traceE "eAddLocation" eAddLocation
                                traceE "eUpdateLocation" (fst <$> eUpdateLocation)
                                traceE "eRemoveLocation" eRemoveLocation

                                -- AddSound
                                -- Read sound file info (a)synchronously
                                {-(eSoundFileInfo, fSoundFileInfo) <- newAsyncEvent-}
                                (eSoundFileInfo, fSoundFileInfo) <- newSyncEvent
                                -- FIXME: Catch exception and report error when file not found.
                                R.reactimate $ (\(i, s) -> fSoundFileInfo $ addSound i s) <$> eAddSound
                                -- Update sound map
                                let eSounds = scanlE (flip (uncurry H.insert)) H.empty eSoundFileInfo
                                    bSounds = R.stepper H.empty eSounds
                                -- Return status
                                R.reactimate $ send Ok <$ eSounds
                                {-R.reactimate $ print <$> eSounds-}
                                {-R.reactimate $ print <$> eSoundFileInfo-}
                                {-R.reactimate $ print <$> eAddSound-}

                                -- Locations and states
                                let (eLocations, bLocations) = hashMapB eAddLocation eRemoveLocation eUpdateLocation

                                (eAddLocationState, fAddLocationState) <- R.newEvent
                                (eRemoveLocationState, fRemoveLocationState) <- R.newEvent
                                (eUpdateLocationState, fUpdateLocationState) <- R.newEvent
                                let (eLocationStates, bLocationStates) = hashMapB (second snd <$> eAddLocationState)
                                                                                  eRemoveLocationState
                                                                                  eUpdateLocationState
                                R.reactimate $ fmap (executeWith engine fAddLocationState) $
                                    uncurry <$> (newLocation <$> bSounds) <@> eAddLocation
                                R.reactimate $ fmap (executeWith engine fRemoveLocationState) $
                                    uncurry freeLocation <$> lookupB bLocationStates eRemoveLocation
                                R.reactimate $ fmap (executeWith engine fUpdateLocationState) $
                                    uncurry . uncurry <$> (updateLocation <$> bSounds)
                                        -- FIXME: Untangle this mess by grouping data by dependencies
                                        <@> ((\h1 h2 (k, f) -> ((k, f (h1 H.! k)), h2 H.! k)) <$> bLocations <*> bLocationStates <@> eUpdateLocation)

                                R.reactimate $ send Ok <$ R.unions [ void eAddLocationState
                                                                   , void eRemoveLocationState
                                                                   , void eUpdateLocationState ]
                                traceE "bLocationStates" eLocationStates

                                -- Listeners and listener states
                                let eAddListenerRewriteUrl =
                                      if local ^$ opts
                                      then second (\l -> let u = streamURL l
                                                         in l { streamURL = case URL.url_type u of
                                                                              URL.Absolute host -> u { URL.url_type = URL.Absolute (host { URL.host = "localhost" }) }
                                                                              _ -> u
                                                              }) <$> eAddListener
                                      else eAddListener
                                R.reactimate $ logLn . URL.exportURL . streamURL . snd <$> eAddListenerRewriteUrl

                                let (eListeners, bListeners) = hashMapB eAddListener eRemoveListener eUpdateListener

                                (eAddListenerState, fAddListenerState) <- R.newEvent
                                (eRemoveListenerState, fRemoveListenerState) <- R.newEvent
                                (eUpdateListenerState, fUpdateListenerState) <- R.newEvent
                                let (eListenerStates, bListenerStates) = hashMapB (second snd <$> eAddListenerState)
                                                                                  eRemoveListenerState
                                                                                  eUpdateListenerState
                                R.reactimate $ fmap (executeWith engine fAddListenerState) $
                                    uncurry <$> (newListener env <$> bLocations <*> bLocationStates <*> bListenerStates) <@> eAddListenerRewriteUrl
                                R.reactimate $ fmap (executeWith engine fRemoveListenerState) $
                                    uncurry freeListener <$> lookupB bListenerStates eRemoveListener

                                R.reactimate $ send Ok <$ R.unions [ void eAddListenerState
                                                                   , void eRemoveListenerState
                                                                   , void eUpdateListenerState ]

                                -- Listener/location map
                                let -- eListenerLocations :: R.Event t (H.HashMap ListenerId (H.HashMap LocationId LocationId))
                                    eListenersOrLocationsChanged = eListeners `R.union` (const <$> bListeners <@> eLocations)
                                    eListenerLocations = fmap . flip listenerLocations <$> bLocations <@> eListenersOrLocationsChanged
                                    bListenerLocations = R.stepper H.empty eListenerLocations
                                    locationEvents ls (_, ls') = (H.foldlWithKey' f [] ls ++ H.foldlWithKey' g [] ls', ls)
                                      where f es l locs =
                                              case H.lookup l ls' of
                                                Nothing ->
                                                  -- New listener: generate 'Enter' events for all locations
                                                  es ++ (Enter l <$> H.keys locs)
                                                Just locs' ->
                                                  -- Examine previous locations
                                                  es
                                                  ++ (Enter l <$> H.keys (H.difference locs locs'))
                                                  ++ (Leave l <$> H.keys (H.difference locs' locs))
                                            g es l locs
                                              | not (H.member l ls) = es ++ (Leave l <$> H.keys locs)
                                              | otherwise = []
                                    eLocationEvents = R.spill
                                                    $ R.filterE (not.null)
                                                    $ fmap fst
                                                    $ R.accumE ([], H.empty)
                                                               (locationEvents <$> eListenerLocations)

                                traceE "eLocationEvents" eLocationEvents

                                -- Start and stop event players
                                (eUpdatedEventPlayers, fUpdatedEventPlayers) <- R.newEvent
                                let bEventPlayers = R.stepper H.empty eUpdatedEventPlayers
                                R.reactimate $ fmap (executeWith engine fUpdatedEventPlayers)
                                             $ updateEventPlayers
                                               <$> bSounds
                                               <*> bLocations
                                               <*> bListeners
                                               <*> bListenerStates
                                               <*> bEventPlayers
                                               <@> eLocationEvents
                                -- Update event players
                                R.reactimate $ fmap (executeWith engine fUpdatedEventPlayers)
                                             $ updateEventPlayersForListener
                                               <$> bLocations
                                               <*> bListeners
                                               <*> bEventPlayers
                                               <@> (fst <$> eUpdateListener)

                                -- Piggedipatchables
                                (eUpdatePatchCables, fUpdatePatchCables) <- R.newEvent
                                let bPatchCables = R.accumB H.empty eUpdatePatchCables
                                    eUpdatePatchCables' = R.unions [
                                        -- AddListener: Add patch cables between new listener and all locations
                                        (\locations locationStates (listenerId, (listener, listenerState)) -> do
                                          group <- SC.rootNode
                                          cables <- fmap H.fromList
                                                      $ T.forM (H.keys locations)
                                                      $ \locationId -> fmap ((,)locationId)
                                                      $ newPatchCable group
                                                                      (bus (locationStates H.! locationId))
                                                                      (outputBus listenerState)
                                                                      listener
                                                                      (locations H.! locationId)
                                          return $ H.insert listenerId cables)
                                        <$> bLocations
                                        <*> bLocationStates
                                        <@> eAddListenerState
                                        -- UpdateListener: Update patch cables with new listener position
                                      , updatePatchCablesForListener
                                          <$> bLocations
                                          <*> bListeners
                                          <*> bPatchCables
                                          <@> (fst <$> eUpdateListener)
                                        -- RemoveListener: Remove patch cables between listener and all locations
                                      , (\patchCables listenerId -> do
                                          SC.exec immediately $ F.forM_ (patchCables H.! listenerId) SC.n_free
                                          return $ H.delete listenerId)
                                        <$> bPatchCables
                                        <@> eRemoveListener
                                        -- AddLocation: Add patch cables between the new location and each listener
                                      , (\listeners listenerStates (locationId, (location, locationState)) -> do
                                          group <- SC.rootNode
                                          updates <- flip H.traverseWithKey listeners $ \listenerId listener -> do
                                            synth <- newPatchCable
                                                      group
                                                      (bus locationState)
                                                      (outputBus (listenerStates H.! listenerId))
                                                      listener
                                                      location
                                            return $ H.insert locationId synth
                                          return $ flip (H.foldlWithKey' (\h k f -> H.adjust f k h))
                                                        updates)
                                        <$> bListeners
                                        <*> bListenerStates
                                        <@> eAddLocationState
                                        -- UpdateLocation: Update patch cables with new location position
                                      , updatePatchCablesForLocation
                                          <$> bLocations
                                          <*> bListeners
                                          <*> bPatchCables
                                          <@> (fst <$> eUpdateLocation)
                                        -- RemoveLocation: Remove patch cables netween the location and each listener
                                      , (\patchCables locationId -> do
                                          SC.exec immediately $ mapM_ SC.n_free $ catMaybes $ H.elems $ fmap (H.lookup locationId) patchCables
                                          return $ fmap (H.delete locationId))
                                        <$> bPatchCables
                                        <@> eRemoveLocation
                                      ]
                                R.reactimate $ fmap (executeWith engine fUpdatePatchCables) eUpdatePatchCables'
                                traceE "bPatchCables" =<< R.changes bPatchCables

                                -- Quit
                                (eFreedListeners, fFreedListeners) <- R.newEvent
                                R.reactimate $ fmap (executeWith engine fFreedListeners) $ const . F.mapM_ (uncurry freeListener) . H.toList <$> bListenerStates <@> eQuit
                                R.reactimate $ (quit engine >> send Ok) <$ eFreedListeners
                            -- Compile network and get event sinks
                            network <- R.compile networkDescription
                            sinks <- MVar.takeMVar eventSinks
                            -- Set up interrupt handler
                            catchSignals handledSignals $ fireQuit sinks ()
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
