{-# LANGUAGE TemplateHaskell #-}
import           Control.Applicative ((<$>))
import           Control.Monad
import           Development.Shake
import           Development.Shake.FilePath
import           Data.Char (toLower)
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

data Env = Env {
    _buildPrefix :: FilePath
  } deriving (Show)

$( makeLens ''Env )

mkEnv :: FilePath -> Env
mkEnv = Env

data Target =
    IOS
  | IOS_Simulator
  | MacOSX
  deriving (Eq, Show)

targetString :: Target -> String
targetString = map toLower . show


data CTarget = CTarget {
    _buildTarget :: Target
  , _buildArch :: String
  } deriving (Show)

$( makeLens ''CTarget )

mkCTarget :: Target -> String -> CTarget
mkCTarget target arch = CTarget target arch

buildDir :: Env -> CTarget -> FilePath
buildDir env target = (buildPrefix ^$ env) </> (targetString (buildTarget ^$ target)) </> (buildArch ^$ target)


data CToolChain = CToolChain {
    _platformPrefix :: FilePath -- Apple-specific?
  , _prefix :: FilePath
  , _compiler :: String
  , _linker :: String
  } deriving (Show)

$( makeLenses [''CToolChain] )

defaultCToolChain :: CToolChain
defaultCToolChain =
    CToolChain {
        _platformPrefix = "/"
      , _prefix = "/"
      , _compiler = "gcc"
      , _linker = "ld"
      }

tool :: (Lens CToolChain String) -> CToolChain -> FilePath
tool f toolChain = (platformPrefix ^$ toolChain) </> (prefix ^$ toolChain) </> "bin" </> (f ^$ toolChain)


data CBuildFlags = CBuildFlags {
    _systemIncludes :: [FilePath]
  , _userIncludes :: [FilePath]
  , _defines :: [(String, Maybe String)]
  , _preprocessorFlags :: [String]
  , _compilerFlags :: [String]
  , _linkerFlags :: [String]
  } deriving (Show)

$( makeLenses [''CBuildFlags] )

defaultCBuildFlags :: CBuildFlags
defaultCBuildFlags =
    CBuildFlags {
        _systemIncludes = []
      , _userIncludes = []
      , _defines = []
      , _preprocessorFlags = []
      , _compilerFlags = []
      , _linkerFlags = []
      }

defineFlags :: CBuildFlags -> [String]
defineFlags = flags "-D" . map (\(a, b) -> maybe a (\b -> a++"="++b) b) . getL defines

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

dependencyFile :: CTarget -> CToolChain -> CBuildFlags -> FilePath -> FilePath -> Rules ()
dependencyFile env toolChain build input output = do
    output ?=> \_ -> do
        need [input]
        systemLoud (tool compiler toolChain)
                $  flag_ "-arch" (buildArch ^$ env)
                ++ flags "-I" (systemIncludes ^$ build)
                ++ flags_ "-iquote" (userIncludes ^$ build)
                ++ (defineFlags build)
                ++ (preprocessorFlags ^$ build)
                ++ ["-MM", "-o", output, input]

parseDependencies :: String -> [FilePath]
parseDependencies = drop 2 . words . filter (/= '\\')

staticObject :: CTarget -> CToolChain -> CBuildFlags -> FilePath -> FilePath -> Rules ()
staticObject env toolChain build input output = do
    let dep = output <.> "d"
    dependencyFile env toolChain build input dep
    output ?=> \_ ->  do
        need [input]
        need =<< parseDependencies <$> readFile' dep
        systemLoud (tool compiler toolChain)
                $  flag_ "-arch" (buildArch ^$ env)
                ++ flags "-I" (systemIncludes ^$ build)
                ++ flags_ "-iquote" (userIncludes ^$ build)
                ++ (defineFlags build)
                ++ (preprocessorFlags ^$ build)
                ++ (compilerFlags ^$ build)
                ++ ["-c", "-o", output, input]

linkC :: CTarget -> CToolChain -> CBuildFlags -> [FilePath] -> FilePath -> Action ()
linkC env toolChain build inputs output = do
    need inputs
    systemLoud (tool linker toolChain)
          $  flag_ "-arch_only" (buildArch ^$ env)
          ++ linkerFlags `getL` build
          ++ flag_ "-o" output
          ++ inputs

data StaticLibrary = StaticLibrary {
    name :: String
  , sources :: CBuildFlags -> FilePath -> Rules (CBuildFlags, [FilePath])
  }

files xs build _ = return (build, xs)

libName :: StaticLibrary -> String
libName = ("lib"++) . flip replaceExtension "a" . name

libBuildDir :: Env -> CTarget -> StaticLibrary -> FilePath
libBuildDir env target lib = buildDir env target </> name lib

libBuildPath :: Env -> CTarget -> StaticLibrary -> FilePath
libBuildPath env target lib = buildDir env target </> libName lib

staticLibrary :: Env -> CTarget -> CToolChain -> CBuildFlags -> StaticLibrary -> Rules ()
staticLibrary env target toolChain build lib = do
    let buildDir = libBuildDir env target lib
    (build', src) <- sources lib build buildDir
    let objects = map (combine buildDir . makeRelative buildDir . (<.> "o")) src
    zipWithM_ (staticObject target toolChain build') src objects
    libBuildPath env target lib ?=> do
        linkC target toolChain (linkerFlags `appendL` ["-static"] $ build') objects

-- ====================================================================
-- Target and build settings

cToolChain_IOS_Simulator =
    platformPrefix ^= "/Developer/Platforms/iPhoneSimulator.platform"
  $ prefix ^= "Developer/usr"
  $ compiler ^= "clang"
  $ linker ^= "libtool"
  $ defaultCToolChain

cToolChain_MacOSX =
    platformPrefix ^= "/Developer/Platforms/MacOSX.platform"
  $ prefix ^= "/usr"
  $ compiler ^= "clang"
  $ linker ^= "libtool"
  $ defaultCToolChain

cBuildFlags_IOS_Simulator =
    appendL defines [("__IPHONE_OS_VERSION_MIN_REQUIRED", Just "40200")]
  $ appendL preprocessorFlags [ "-isysroot", platformPrefix `combineL` "Developer/SDKs/iPhoneSimulator5.0.sdk" $ cToolChain_IOS_Simulator ]
  $ appendL compilerFlags (flags "-f" ["visibility=hidden", "visibility-inlines-hidden"]
                            ++ flag "-g" "dwarf-2")
  $ defaultCBuildFlags

cBuildFlags_MacOSX =
    appendL compilerFlags (flags "-f" ["visibility=hidden", "visibility-inlines-hidden"]
                            ++ flag "-g" "dwarf-2")
  $ defaultCBuildFlags

-- ====================================================================
-- Library

serdDir = "external_libraries/serd-0.5.0"
sordDir = "external_libraries/sord-0.5.0"
lilvDir = "external_libraries/lilv-0.5.0"
boostDir = "external_libraries/boost"

mescalineBuildFlags env buildFlags =
    userIncludes `appendL`
      ( ["."]
     ++ [ "external_libraries" ]
     ++ [ "external_libraries/lv2" ]
     ++ case buildTarget ^$ env of
            IOS           -> [ "platform/ios" ]
            IOS_Simulator -> [ "platform/ios" ]
            _             -> []
     ++ [ serdDir, sordDir, sordDir </> "src", lilvDir, lilvDir </> "src" ] )
  $ systemIncludes `appendL`
       ( [ "src" ]
      ++ [ boostDir
         , "external_libraries/boost_lockfree" ] )
  $ buildFlags

mescalineLib :: CTarget -> IO StaticLibrary
mescalineLib env = do
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
     ++ (if (buildTarget ^$ env) `elem` [IOS, IOS_Simulator]
         then under "platform/ios" [ "Mescaline/Audio/IO/RemoteIODriver.cpp" ]
         else [])

libMescaline :: CTarget -> CBuildFlags -> IO (CBuildFlags, StaticLibrary)
libMescaline env buildFlags = do
    lib <- mescalineLib env
    return (mescalineBuildFlags env buildFlags, lib)

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

processTargets shake env = mapM_ processTarget where
    processTarget t =
        case t of
            "clean" -> removeDirectoryRecursive (buildPrefix ^$ env)
            "ios-simulator" -> do
                let target = mkCTarget IOS_Simulator "i386"
                    toolChain = cToolChain_IOS_Simulator
                    buildFlags = cBuildFlags_IOS_Simulator
                mescaline <- libMescaline target buildFlags
                shake $ do
                    let libs = [ mescaline ]
                        lib = uncurry (staticLibrary env target toolChain)
                        libFile = libBuildPath env target . snd
                    mapM_ lib libs
                    want (map libFile libs)
            "macosx" -> do
                let target = mkCTarget MacOSX "x86_64"
                    toolChain = cToolChain_MacOSX
                    buildFlags = cBuildFlags_MacOSX
                mescaline <- libMescaline target buildFlags
                shake $ do
                    let libs = [ mescaline ]
                        lib = uncurry (staticLibrary env target toolChain)
                        libFile = libBuildPath env target . snd
                    mapM_ lib libs
                    want (map libFile libs)

main = do
    opts <- processArgs arguments
    let shakeIt = shake (optionsToShake opts)
    if help ^$ opts
        then print $ helpText [] HelpFormatDefault arguments
        else case targets ^$ opts of
                [] -> do
                    putStrLn "Please chose a target"
                ts -> processTargets shakeIt (mkEnv (output ^$ opts)) ts
