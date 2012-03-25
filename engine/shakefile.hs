{-# LANGUAGE TemplateHaskell #-}
import           Control.Applicative ((<$>))
import           Control.Monad
import           Development.Shake
import           Development.Shake.FilePath
import           Data.Lens.Common
import           Data.Lens.Template
import           Data.List (intersperse, isInfixOf, isSuffixOf)
import           Data.Maybe
import           GHC.Conc (numCapabilities)
import qualified System.Console.CmdArgs.Implicit as C
import           System.Console.CmdArgs.Explicit
import           System.Directory (removeDirectoryRecursive)
import           System.Environment
import           System.FilePath.Find

under :: FilePath -> [FilePath] -> [FilePath]
under dir = map prepend
    where prepend ""   = dir
          prepend "."  = dir
          prepend ".." = takeDirectory dir
          prepend x    = combine dir x

flag_ :: String -> String -> [String]
flag_ o x = [o, x]

flags_ :: String -> [String] -> [String]
flags_ o = concat . map (flag_ o)

flag :: String -> String -> [String]
flag f x = [f++x]

flags :: String -> [String] -> [String]
flags f = map (f++)

appendL :: Lens a [b] -> [b] -> a -> a
appendL l bs a = setL l (getL l a ++ bs) a

combineL :: Lens a FilePath -> FilePath -> a -> FilePath
combineL l p a = getL l a </> p

(?=>) :: FilePath -> (FilePath -> Action ()) -> Rules ()
f ?=> a = (==f) ?> a

data CToolChain = CToolChain {
    _platform :: String
  , _platformPrefix :: FilePath
  , _prefix :: FilePath
  , _compiler :: String
  , _linker :: String
  } deriving (Show)

$( makeLenses [''CToolChain] )

mkCToolChain :: String -> CToolChain
mkCToolChain platform =
    CToolChain {
        _platform = platform
      , _platformPrefix = "/"
      , _prefix = "/"
      , _compiler = "gcc"
      , _linker = "ld"
      }

tool :: (Lens CToolChain String) -> CToolChain -> FilePath
tool f toolChain = (platformPrefix ^$ toolChain) </> (prefix ^$ toolChain) </> "bin" </> (f ^$ toolChain)

data CBuild = CBuild {
    _buildArch :: String
  , _buildPrefix :: String
  , _systemIncludes :: [FilePath]
  , _userIncludes :: [FilePath]
  , _defines :: [(String, Maybe String)]
  , _preprocessorFlags :: [String]
  , _compilerFlags :: [String]
  , _linkerFlags :: [String]
  } deriving (Show)

$( makeLenses [''CBuild] )

mkCBuild :: String -> CBuild
mkCBuild arch =
    CBuild {
        _buildArch = arch
      , _buildPrefix = "./build"
      , _systemIncludes = []
      , _userIncludes = []
      , _defines = []
      , _preprocessorFlags = []
      , _compilerFlags = []
      , _linkerFlags = []
      }

defineFlags :: CBuild -> [String]
defineFlags = flags "-D" . map (\(a, b) -> maybe a (\b -> a++"="++b) b) . getL defines

buildDir :: CToolChain -> CBuild -> FilePath
buildDir toolChain build = (buildPrefix ^$ build) </> (platform ^$ toolChain) </> (buildArch ^$ build)

systemLoud :: FilePath -> [String] -> Action ()
systemLoud cmd args = do
    putNormal $ unwords $ [cmd] ++ args
    system' cmd args

sed :: String -> FilePath -> FilePath -> Action ()
sed command input output = do
    need [input]
    (stdout, stderr) <- systemOutput "sed" ["-e", command, input]
    writeFile' output stdout

sourceTransform :: (FilePath -> FilePath) -> String -> FilePath -> Rules FilePath
sourceTransform f cmd input = do
    let output = f input
    output ?=> sed cmd input
    want [output]
    return output

dependencyFile :: CToolChain -> CBuild -> FilePath -> FilePath -> Rules ()
dependencyFile toolChain build input output = do
    output ?=> \_ -> do
        need [input]
        systemLoud (tool compiler toolChain)
                $  flag_ "-arch" (buildArch ^$ build)
                ++ flags "-I" (systemIncludes ^$ build)
                ++ flags_ "-iquote" (userIncludes ^$ build)
                ++ (defineFlags build)
                ++ (preprocessorFlags ^$ build)
                ++ ["-MM", "-o", output, input]

parseDependencies :: String -> [FilePath]
parseDependencies = drop 2 . words . filter (/= '\\')

staticObject :: CToolChain -> CBuild -> FilePath -> FilePath -> Rules ()
staticObject toolChain build input output = do
    let dep = output <.> "d"
    dependencyFile toolChain build input dep
    output ?=> \_ ->  do
        need [input]
        need =<< parseDependencies <$> readFile' dep
        systemLoud (tool compiler toolChain)
                $  flag_ "-arch" (buildArch ^$ build)
                ++ flags "-I" (systemIncludes ^$ build)
                ++ flags_ "-iquote" (userIncludes ^$ build)
                ++ (defineFlags build)
                ++ (preprocessorFlags ^$ build)
                ++ (compilerFlags ^$ build)
                ++ ["-c", "-o", output, input]

linkC :: CToolChain -> CBuild -> [FilePath] -> FilePath -> Action ()
linkC toolChain build inputs output = do
    need inputs
    systemLoud (tool linker toolChain)
          $  flag_ "-arch_only" (buildArch ^$ build)
          ++ linkerFlags `getL` build
          ++ flag_ "-o" output
          ++ inputs

data StaticLibrary = StaticLibrary {
    name :: String
  , sources :: CBuild -> FilePath -> Rules (CBuild, [FilePath])
  }

files xs build _ = return (build, xs)

libName :: StaticLibrary -> String
libName = ("lib"++) . flip replaceExtension "a" . name

libBuildDir :: CToolChain -> CBuild -> StaticLibrary -> FilePath
libBuildDir toolChain build lib = buildDir toolChain build </> name lib

libBuildPath :: CToolChain -> CBuild -> StaticLibrary -> FilePath
libBuildPath toolChain build lib = buildDir toolChain build </> libName lib

staticLibrary :: CToolChain -> CBuild -> StaticLibrary -> Rules ()
staticLibrary toolChain build lib = do
    let buildDir = libBuildDir toolChain build lib
    (build', src) <- sources lib build buildDir
    let objects = map (combine buildDir . makeRelative buildDir . (<.> "o")) src
    zipWithM_ (staticObject toolChain build') src objects
    libBuildPath toolChain build lib ?=> do
        linkC toolChain (linkerFlags `appendL` ["-static"] $ build') objects

-- ====================================================================
-- Target and build settings

cToolChain_IOS_Simulator =
    platformPrefix ^= "/Developer/Platforms/iPhoneSimulator.platform"
  $ prefix ^= "Developer/usr"
  $ compiler ^= "clang"
  $ linker ^= "libtool"
  $ mkCToolChain "iOS-Simulator"

cToolChain_MacOSX = CToolChain {
    _platform = "macosx"
  , _platformPrefix = "/Developer/Platforms/MacOSX.platform"
  , _prefix = "/usr"
  , _compiler = "clang"
  , _linker = "libtool"
  }

cBuild_IOS_Simulator = CBuild {
    _buildArch = "i386"
  , _buildPrefix = "./build"
  , _systemIncludes = []
  , _userIncludes = []
  , _defines = [("__IPHONE_OS_VERSION_MIN_REQUIRED", Just "40200")]
  , _preprocessorFlags = [ "-isysroot", platformPrefix `combineL` "Developer/SDKs/iPhoneSimulator5.0.sdk" $ cToolChain_IOS_Simulator ]
  , _compilerFlags = flags "-f" ["visibility=hidden", "visibility-inlines-hidden"]
                 ++ flag "-g" "dwarf-2"
  , _linkerFlags = []
  }

cBuild_MacOSX = CBuild {
    _buildArch = "x86_64"
  , _buildPrefix = "./build"
  , _systemIncludes = []
  , _userIncludes = []
  , _defines = []
  , _preprocessorFlags = []
  , _compilerFlags = flags "-f" ["visibility=hidden", "visibility-inlines-hidden"]
                 ++ flag "-g" "dwarf-2"
  , _linkerFlags = []
  }

data Target =
    IOS
  | IOS_Simulator
  | MacOSX
  deriving (Eq, Show)

buildTarget :: Target
buildTarget = IOS_Simulator

target :: Target -> a -> a -> a
target t a1 a2 =
    if buildTarget == t
    then a1
    else a2

cToolChain =
    case buildTarget of
        IOS_Simulator -> cToolChain_IOS_Simulator
        MacOSX -> cToolChain_MacOSX

cBuild =
    case buildTarget of
        IOS_Simulator -> cBuild_IOS_Simulator
        MacOSX -> cBuild_MacOSX

-- ====================================================================
-- Library

serdDir = "external_libraries/serd-0.5.0"
sordDir = "external_libraries/sord-0.5.0"
lilvDir = "external_libraries/lilv-0.5.0"
boostDir = "external_libraries/boost"

mescalineBuild =
    userIncludes `appendL`
      ( ["."]
     ++ [ "external_libraries" ]
     ++ case buildTarget of
            IOS           -> [ "platform/ios" ]
            IOS_Simulator -> [ "platform/ios" ]
            _             -> []
     ++ [ "/usr/local/include" ] -- Gnargh
     ++ [ serdDir, sordDir, sordDir </> "src", lilvDir, lilvDir </> "src" ] )
  $ systemIncludes `appendL`
       ( [ "src" ]
      ++ [ boostDir
         , "external_libraries/boost_lockfree" ] )
  $ cBuild

mescalineLib = do
    boostSrc <- find always
                    (extension ==? ".cpp" &&?
                     (not . isSuffixOf "win32") <$> directory &&?
                     (not . isSuffixOf "test/src") <$> directory &&?
                     (fileName /=? "utf8_codecvt_facet.cpp"))
                    boostDir
    return $ StaticLibrary "mescaline" $ files $
        -- serd
        under (serdDir </> "src") [
            "env.c"
          , "error.c"
          , "node.c"
          , "reader.c"
          , "uri.c"
          , "writer.c"
          ]
        -- sord
     ++ under (sordDir </> "src") [
            "sord.c"
          , "syntax.c"
          , "zix/tree.c"
          , "zix/hash.c"
          ]
        -- lilv
     ++ under (lilvDir </> "src") [
            "collections.c"
          , "instance.c"
          , "node.c"
          , "plugin.c"
          , "pluginclass.c"
          , "port.c"
          , "query.c"
          , "scalepoint.c"
          , "ui.c"
          , "util.c"
          , "world.c"
          , "zix/tree.c"
          ]
        -- boost
     ++ boostSrc
        -- engine
     ++ under "src" [
            "Mescaline/Audio/AudioBus.cpp"
          , "Mescaline/Audio/Client.cpp"
          , "Mescaline/Audio/Engine.cpp"
          , "Mescaline/Audio/Group.cpp"
          , "Mescaline/Audio/Node.cpp"
          , "Mescaline/Audio/Resource.cpp"
          , "Mescaline/Audio/Synth.cpp"
          , "Mescaline/Audio/SynthDef.cpp"
          , "Mescaline/Memory/Manager.cpp"
          , "Mescaline/Memory.cpp"
          ]
        -- plugins
     ++ [ "lv2/puesnada.es/plugins/sine.lv2/sine.cpp" ]
        -- platform dependent
     ++ (if buildTarget `elem` [IOS, IOS_Simulator]
         then under "platform/ios" [ "Mescaline/Audio/IO/RemoteIODriver.cpp" ]
         else [])

libMescaline = do
    lib <- mescalineLib
    return (mescalineBuild, lib)

getShakeOptions :: FilePath -> IO ShakeOptions
getShakeOptions buildDir = do
    nc <- return numCapabilities -- This has been changed to Control.Concurrent.getNumCapabilities in 7.?
    return $ shakeOptions {
        shakeVerbosity = Normal
      , shakeThreads = nc
      , shakeReport = Just (buildDir </> "report.html")
      }

data Options = Options {
    _help :: Bool
  , _verbosity :: Verbosity
  , _jobs :: Int
  , _output :: FilePath
  , _report :: Bool
  , _targets :: [String]
  } deriving (Show)

defaultOptions = Options {
    _help = False
  , _verbosity = Normal
  , _jobs = 1
  , _output = "./build"
  , _report = False
  , _targets = []
  }

$( makeLenses [''Options] )
 
arguments :: Mode Options
arguments =
    mode "shake" defaultOptions "Shake build system"
         (flagArg (\t -> Right . modL targets (++[t])) "TARGET..") $
         [ flagHelpSimple (setL help True)
         , flagReq ["verbosity","v"] (upd verbosity . read) "VERBOSITY" "Verbosity"
         , flagOpt "1" ["jobs","j"] (upd jobs . read) "NUMBER" "Number of parallel jobs"
         , flagReq ["output", "o"] (upd output) "DIRECTORY" "Build products output directory"
         , flagBool ["report", "r"] (setL report) "Generate build report"
         ]
          -- ++ flagsVerbosity (setL verbosity)
    where upd what x = Right . setL what x

optionsToShake opts = shakeOptions {
    shakeThreads = jobs ^$ opts
  , shakeVerbosity = verbosity ^$ opts
  , shakeReport = if report ^$ opts
                    then Just $ (output ^$ opts) </> "shake.html"
                    else Nothing
  }

processTargets shake build [] = return ()
processTargets shake build (t:ts) = do
    case t of
        "clean" -> removeDirectoryRecursive (buildPrefix ^$ build)
    processTargets shake build ts

main = do
    opts <- processArgs arguments
    let shakeIt = shake (optionsToShake opts)
    if help ^$ opts
        then print $ helpText [] HelpFormatDefault arguments
        else case targets ^$ opts of
                [] -> do
                    mescaline <- libMescaline
                    shakeIt $ do
                        let libs = [ mescaline ]
                            lib = uncurry (staticLibrary cToolChain)
                            libFile = uncurry (libBuildPath cToolChain)
                        mapM_ lib libs
                        want (map libFile libs)
                ts -> processTargets shakeIt cBuild ts
