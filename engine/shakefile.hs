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
import           System.Process (readProcess)

under :: FilePath -> [FilePath] -> [FilePath]
under dir = map prepend
    where prepend ""   = dir
          prepend "."  = dir
          prepend ".." = takeDirectory dir
          prepend x    = combine dir x

flag_ :: String -> String -> [String]
flag_ o x = [o, x]

flag :: String -> [String]
flag f = [f]

flags_ :: String -> [String] -> [String]
flags_ o = concat . map (flag_ o)

flags :: String -> [String] -> [String]
flags f = map (f++)

appendL :: Lens a [b] -> [b] -> a -> a
appendL l bs a = setL l (getL l a ++ bs) a

combineL :: Lens a FilePath -> FilePath -> a -> FilePath
combineL l p a = getL l a </> p

(?=>) :: FilePath -> (FilePath -> Action ()) -> Rules ()
f ?=> a = (==f) ?> a

data Env = Env {
    _buildConfiguration :: String
  , _buildPrefix :: FilePath
  } deriving (Show)

$( makeLens ''Env )

mkEnv :: String -> FilePath -> Env
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
buildDir env target = (buildPrefix ^$ env)
                  </> map toLower (buildConfiguration ^$ env)
                  </> (targetString (buildTarget ^$ target))
                  </> (buildArch ^$ target)


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
tool f toolChain = (prefix ^$ toolChain) </> "bin" </> (f ^$ toolChain)


data CBuildFlags = CBuildFlags {
    _systemIncludes :: [FilePath]
  , _userIncludes :: [FilePath]
  , _defines :: [(String, Maybe String)]
  , _preprocessorFlags :: [String]
  , _compilerFlags :: [String]
  , _libraryPath :: [FilePath]
  , _libraries :: [String]
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
      , _libraryPath = []
      , _libraries = []
      , _linkerFlags = []
      }

defineFlags :: CBuildFlags -> [String]
defineFlags = flags "-D" . map (\(a, b) -> maybe a (\b -> a++"="++b) b) . getL defines


type CBuildEnv = (CToolChain, CBuildFlags)


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
dependencyFile target toolChain build input output = do
    output ?=> \_ -> do
        need [input]
        systemLoud (tool compiler toolChain)
                $  flag_ "-arch" (buildArch ^$ target)
                ++ flags "-I" (systemIncludes ^$ build)
                ++ flags_ "-iquote" (userIncludes ^$ build)
                ++ (defineFlags build)
                ++ (preprocessorFlags ^$ build)
                ++ ["-MM", "-o", output, input]

parseDependencies :: String -> [FilePath]
parseDependencies = drop 2 . words . filter (/= '\\')

type ObjectRule = CTarget -> CToolChain -> CBuildFlags -> FilePath -> [FilePath] -> FilePath -> Rules ()

staticObject :: ObjectRule
staticObject target toolChain buildFlags input deps output = do
    let depFile = output <.> "d"
    dependencyFile target toolChain buildFlags input depFile
    output ?=> \_ ->  do
        deps' <- parseDependencies <$> readFile' depFile
        need $ [input] ++ deps ++ deps'
        systemLoud (tool compiler toolChain)
                $  flag_ "-arch" (buildArch ^$ target)
                ++ flags "-I" (systemIncludes ^$ buildFlags)
                ++ flags_ "-iquote" (userIncludes ^$ buildFlags)
                ++ (defineFlags buildFlags)
                ++ (preprocessorFlags ^$ buildFlags)
                ++ (compilerFlags ^$ buildFlags)
                ++ ["-c", "-o", output, input]

sharedObject :: ObjectRule
sharedObject target toolChain = staticObject target toolChain . appendL compilerFlags (flag "-fPIC")

type Linker = CTarget -> CToolChain -> CBuildFlags -> [FilePath] -> FilePath -> Action ()

link :: Linker
link target toolChain buildFlags inputs output = do
    need inputs
    systemLoud (tool linker toolChain)
          $  flag_ "-arch_only" (buildArch ^$ target)
          ++ linkerFlags `getL` buildFlags
          ++ flags "-L" (libraryPath ^$ buildFlags)
          ++ flags "-l" (libraries ^$ buildFlags)
          ++ flag_ "-o" output
          ++ inputs

linkStaticLibrary :: Linker
linkStaticLibrary target toolChain = link target toolChain . appendL linkerFlags [ "-static" ]

linkSharedLibrary :: Linker
linkSharedLibrary target toolChain = link target toolChain . appendL linkerFlags [ "-shared" ]

linkSharedLibrary_MacOSX :: Linker
linkSharedLibrary_MacOSX target toolChain = link target toolChain . appendL linkerFlags [ "-dynamiclib" ]

data SourceTree b = SourceTree (b -> b) [(FilePath, [FilePath])]

sourceTree :: (b -> b) -> [(FilePath, [FilePath])] -> SourceTree b
sourceTree = SourceTree

sourceFiles :: [FilePath] -> [(FilePath, [FilePath])]
sourceFiles = map (flip (,) [])

data LinkType = Static | Shared deriving (Show)

data Library = Library {
    libName :: String
  , libSources :: [SourceTree CBuildFlags]
  }

staticLibFileName :: String -> FilePath
staticLibFileName = ("lib"++) . (<.> "a")

sharedLibFileName :: String -> FilePath
sharedLibFileName = ("lib"++) . (<.> "dylib")

libBuildDir :: Env -> CTarget -> FilePath -> FilePath
libBuildDir env target libFileName = buildDir env target </> map tr (libFileName)
    where tr '.' = '_'
          tr x   = x

libBuildPath :: Env -> CTarget -> FilePath -> FilePath
libBuildPath env target libFileName = buildDir env target </> libFileName

cLibrary :: ObjectRule -> Linker -> (Library -> FilePath)
         -> Env -> CTarget -> CToolChain -> CBuildFlags
         -> Library
         -> Rules FilePath
cLibrary object link libFileName env target toolChain buildFlags lib = do
    let libFile = libFileName lib
        libPath = libBuildPath env target libFile
        buildDir = libBuildDir env target libFile
    objects <- forM (libSources lib) $ \(SourceTree mapBuildFlags srcTree) -> do
        let src = map fst srcTree
            dep = map snd srcTree
            obj = map (combine buildDir . makeRelative buildDir . (<.> "o")) src
        zipWithM_ ($) (zipWith (object target toolChain (mapBuildFlags buildFlags)) src dep) obj
        return obj
    libPath ?=> link target toolChain buildFlags (concat objects)
    return libPath

staticLibrary :: Env -> CTarget -> CToolChain -> CBuildFlags -> Library -> Rules FilePath
staticLibrary = cLibrary staticObject linkStaticLibrary (staticLibFileName . libName)

sharedLibrary :: Env -> CTarget -> CToolChain -> CBuildFlags -> Library -> Rules FilePath
sharedLibrary = cLibrary sharedObject linkSharedLibrary_MacOSX (sharedLibFileName . libName)

-- ====================================================================
-- Target and build settings

cToolChain_IOS_Simulator :: CToolChain
cToolChain_IOS_Simulator =
    platformPrefix ^= "/Developer/Platforms/iPhoneSimulator.platform/Developer/SDKs/iPhoneSimulator5.0.sdk"
  $ prefix ^= "/Developer/Platforms/iPhoneSimulator.platform/Developer/usr"
  $ compiler ^= "clang"
  $ linker ^= "libtool"
  $ defaultCToolChain

cBuildFlags_IOS_Simulator :: CBuildFlags
cBuildFlags_IOS_Simulator =
    appendL defines [("__IPHONE_OS_VERSION_MIN_REQUIRED", Just "40200")]
  $ appendL preprocessorFlags [ "-isysroot", platformPrefix ^$ cToolChain_IOS_Simulator ]
  $ appendL compilerFlags (flag "-fvisibility=hidden")
  $ defaultCBuildFlags

cToolChain_MacOSX_clang :: CToolChain
cToolChain_MacOSX_clang =
    platformPrefix ^= "/Developer/SDKs/MacOSX10.6.sdk"
  $ prefix ^= "/usr"
  $ compiler ^= "clang"
  $ linker ^= "clang++"
  $ defaultCToolChain

cToolChain_MacOSX_gcc :: CToolChain
cToolChain_MacOSX_gcc =
    platformPrefix ^= (platformPrefix ^$ cToolChain_MacOSX_clang)
  $ prefix ^= (prefix ^$ cToolChain_MacOSX_clang)
  $ compiler ^= "gcc"
  $ linker ^= "g++"
  $ defaultCToolChain

cBuildFlags_MacOSX :: CBuildFlags
cBuildFlags_MacOSX =
    appendL preprocessorFlags [ "-isysroot", platformPrefix ^$ cToolChain_MacOSX_clang ]
  $ appendL compilerFlags (flag "-std=c99" ++ flag "-fvisibility=hidden")
  $ defaultCBuildFlags

-- ====================================================================
-- Library

externalLibraries :: FilePath
externalLibraries = "external_libraries"

externalLibrary :: FilePath -> FilePath
externalLibrary = combine externalLibraries

lv2Dir :: FilePath
lv2Dir = externalLibrary "lv2"

serdDir :: FilePath
serdDir = externalLibrary "serd-0.5.0"

sordDir :: FilePath
sordDir = externalLibrary "sord-0.5.0"

lilvDir :: FilePath
lilvDir = externalLibrary "lilv-0.5.0"

boostDir :: FilePath
boostDir = externalLibrary "boost"

serdBuildFlags :: CBuildFlags -> CBuildFlags
serdBuildFlags = appendL userIncludes
                    [ serdDir, serdDir </> "src" ]

sordBuildFlags :: CBuildFlags -> CBuildFlags
sordBuildFlags = appendL userIncludes
                    [ sordDir, sordDir </> "src"
                    , serdDir
                    , externalLibraries ]

lilvBuildFlags :: CBuildFlags -> CBuildFlags
lilvBuildFlags = appendL userIncludes
                    [ lilvDir, lilvDir </> "src"
                    , serdDir
                    , sordDir
                    , externalLibraries
                    , lv2Dir ]

boostBuildFlags :: CBuildFlags -> CBuildFlags
boostBuildFlags = appendL systemIncludes [ boostDir ]

engineBuildFlags :: CTarget -> CBuildFlags -> CBuildFlags
engineBuildFlags target buildFlags =
    userIncludes `appendL`
      ( ["."]
     ++ [ "external_libraries" ]
     ++ [ "external_libraries/lv2" ]
     ++ case buildTarget ^$ target of
            IOS           -> [ "platform/ios" ]
            IOS_Simulator -> [ "platform/ios" ]
            MacOSX        -> [ "platform/jack" ]
            -- _             -> []
     ++ [ serdDir, sordDir, lilvDir ] )
  $ systemIncludes `appendL`
       ( [ "src" ]
      ++ [ boostDir
         , "external_libraries/boost_lockfree" ] )
  $ buildFlags

mescalineBuildFlags = libraries ^= [ "m" ]

mescalineLib :: CTarget -> IO Library
mescalineLib target = do
    boostSrc <- find always
                    (    extension ==? ".cpp"
                     &&? (not . isSuffixOf "win32") <$> directory
                     &&? (not . isSuffixOf "test/src") <$> directory
                     -- FIXME: linking `filesystem' into a shared library fails with:
                     --     Undefined symbols for architecture x86_64:
                     --       "vtable for boost::filesystem::detail::utf8_codecvt_facet", referenced from:
                     --           boost::filesystem::detail::utf8_codecvt_facet::utf8_codecvt_facet(unsigned long) in v2_path.cpp.o
                     --           boost::filesystem::detail::utf8_codecvt_facet::utf8_codecvt_facet(unsigned long) in path.cpp.o
                     --       NOTE: a missing vtable usually means the first non-inline virtual member function has no definition.
                     &&? (not . elem "filesystem/" . splitPath) <$> filePath
                     &&? (fileName /=? "utf8_codecvt_facet.cpp")
                    )
                    boostDir
    return $ Library "mescaline" $ [
        -- serd
        sourceTree serdBuildFlags $ sourceFiles $
            under (serdDir </> "src") [
                "env.c"
              , "error.c"
              , "node.c"
              , "reader.c"
              , "uri.c"
              , "writer.c"
              ]
        -- sord
      , sourceTree sordBuildFlags $ sourceFiles $
            under (sordDir </> "src") [
                "sord.c"
              , "syntax.c"
              , "zix/tree.c"
              , "zix/hash.c"
              ]
        -- lilv
      , sourceTree lilvBuildFlags $ sourceFiles $
            under (lilvDir </> "src") [
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
      , sourceTree boostBuildFlags $ sourceFiles boostSrc
        -- engine
      , sourceTree (engineBuildFlags target) $ sourceFiles $
            under "src" [
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
            ++ (if (buildTarget ^$ target) `elem` [IOS, IOS_Simulator]
                then under "platform/ios" [ "Mescaline/Audio/IO/RemoteIODriver.cpp" ]
                else if (buildTarget ^$ target) `elem` [MacOSX]
                     then under "platform/jack" [ "Mescaline/API.cpp"
                                                , "Mescaline/Audio/IO/JackDriver.cpp" ]
                     else [])
        ]

-- ====================================================================
-- Configurations

type Configuration = (String, CBuildFlags -> CBuildFlags)

applyConfiguration :: String -> [Configuration] -> CBuildFlags -> CBuildFlags
applyConfiguration c cs =
    case lookup c cs of
        Nothing -> id
        Just f  -> f

applyBuildConfiguration :: Env -> [Configuration] -> CBuildFlags -> CBuildFlags
applyBuildConfiguration env = applyConfiguration (buildConfiguration ^$ env)

configurations :: [Configuration]
configurations = [
    ("release", appendL compilerFlags (flag "-O2"))
  , ("debug", appendL compilerFlags (flag "-gdwarf-2"))
  ]

-- ====================================================================
-- PkgConfig

pkgConfig :: String -> IO (CBuildFlags -> CBuildFlags)
pkgConfig pkg = do
    cflags <- parseFlags <$> readProcess "pkg-config" ["--cflags", pkg] ""
    lflags <- parseFlags <$> readProcess "pkg-config" ["--libs", pkg] ""
    return $ appendL compilerFlags cflags . appendL linkerFlags lflags
    where
        parseFlags = map (dropSuffix "\\") . words . head . lines
        dropSuffix s x = if s `isSuffixOf` x
                         then reverse (drop (length s) (reverse x))
                         else x

-- ====================================================================
-- Commandline options

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
  , _configuration :: String
  , _targets :: [String]
  } deriving (Show)

defaultOptions :: String -> Options
defaultOptions defaultConfig = Options {
    _help = False
  , _verbosity = Normal
  , _jobs = 1
  , _output = "./build"
  , _report = False
  , _configuration = defaultConfig
  , _targets = []
  }

$( makeLenses [''Options] )
 
arguments :: [String] -> [String] -> Mode Options
arguments cs ts =
    mode "shake" (defaultOptions "debug") "Shake build system"
         (flagArg (updList ts targets) "TARGET..") $
         [ flagHelpSimple (setL help True)
         , flagReq ["verbosity","v"] (upd verbosity . read) "VERBOSITY" "Verbosity"
         , flagOpt "1" ["jobs","j"] (upd jobs . read) "NUMBER" "Number of parallel jobs"
         , flagReq ["output", "o"] (upd output) "DIRECTORY" "Build products output directory"
         , flagBool ["report", "r"] (setL report) "Generate build report"
         , flagReq ["config", "c"] (updEnum cs configuration) "CONFIGURATION" "Configuration"
         ]
          -- ++ flagsVerbosity (setL verbosity)
    where
        upd what x = Right . setL what x
        updList xs what x = if x `elem` xs
                            then Right . modL what (++[x])
                            else const $ Left $ show x ++ " not in " ++ show xs
        updEnum xs what x = if x `elem` xs
                            then Right . setL what x
                            else const $ Left $ show x ++ " not in " ++ show xs

optionsToShake :: Options -> ShakeOptions
optionsToShake opts = shakeOptions {
    shakeThreads = jobs ^$ opts
  , shakeVerbosity = verbosity ^$ opts
  , shakeReport = if report ^$ opts
                    then Just $ (output ^$ opts) </> "shake.html"
                    else Nothing
  }

-- ====================================================================
-- Commandline targets

targetSpecs :: [(String, (Rules () -> IO ()) -> Env -> IO ())]
targetSpecs = [
    ( "clean", const (removeDirectoryRecursive . getL buildPrefix) )
  , ( "ios-simulator",
    \shake env -> do
        let target = mkCTarget IOS_Simulator "i386"
            toolChain = cToolChain_IOS_Simulator
            buildFlags = applyBuildConfiguration env configurations cBuildFlags_IOS_Simulator
        libmescaline <- mescalineLib target
        shake $ do
            let libs = [ libmescaline ]
                lib = staticLibrary env target toolChain buildFlags
                libFile = libBuildPath env target
            want =<< mapM lib libs
    )
  , ( "macosx",
    \shake env -> do
        jackBuildFlags <- pkgConfig "jack"
        print (jackBuildFlags cBuildFlags_MacOSX)
        let target = mkCTarget MacOSX "x86_64"
            toolChain = cToolChain_MacOSX_gcc
            buildFlags = (mescalineBuildFlags.jackBuildFlags)
                         (applyBuildConfiguration env configurations cBuildFlags_MacOSX)
        libmescaline <- mescalineLib target
        shake $ do
            let libs = [ libmescaline ]
                lib = sharedLibrary env target toolChain buildFlags
                libFile = libBuildPath env target
            want =<< mapM lib libs
    )
  ]

processTargets :: (Rules () -> IO ()) -> Env -> [String] -> IO ()
processTargets shake env = mapM_ processTarget where
    processTarget t =
        case lookup t targetSpecs of
            Nothing -> putStrLn $ "Warning: Target " ++ t ++ " not found"
            Just f -> f shake env

main :: IO ()
main = do
    let args = arguments (map fst configurations) (map fst targetSpecs)
    opts <- processArgs args
    let shakeIt = shake (optionsToShake opts)
        env = mkEnv (configuration ^$ opts) (output ^$ opts)
    if help ^$ opts
        then print $ helpText [] HelpFormatDefault args
        else case targets ^$ opts of
                [] -> do
                    -- TODO: integrate this with option processing
                    putStrLn $ "Please chose a target:"
                    mapM_ (putStrLn . ("\t"++) . fst) targetSpecs
                ts -> processTargets shakeIt env ts
