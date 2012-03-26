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

staticObject :: CTarget -> CToolChain -> CBuildFlags -> FilePath -> [FilePath] -> FilePath -> Rules ()
staticObject env toolChain buildFlags input deps output = do
    let depFile = output <.> "d"
    dependencyFile env toolChain buildFlags input depFile
    output ?=> \_ ->  do
        deps' <- parseDependencies <$> readFile' depFile
        need $ [input] ++ deps ++ deps'
        systemLoud (tool compiler toolChain)
                $  flag_ "-arch" (buildArch ^$ env)
                ++ flags "-I" (systemIncludes ^$ buildFlags)
                ++ flags_ "-iquote" (userIncludes ^$ buildFlags)
                ++ (defineFlags buildFlags)
                ++ (preprocessorFlags ^$ buildFlags)
                ++ (compilerFlags ^$ buildFlags)
                ++ ["-c", "-o", output, input]

linkC :: CTarget -> CToolChain -> CBuildFlags -> [FilePath] -> FilePath -> Action ()
linkC env toolChain buildFlags inputs output = do
    need inputs
    systemLoud (tool linker toolChain)
          $  flag_ "-arch_only" (buildArch ^$ env)
          ++ linkerFlags `getL` buildFlags
          ++ flag_ "-o" output
          ++ inputs


data SourceTree b = SourceTree (b -> b) [(FilePath, [FilePath])]

sourceTree :: (b -> b) -> [(FilePath, [FilePath])] -> SourceTree b
sourceTree = SourceTree

sourceFiles :: [FilePath] -> [(FilePath, [FilePath])]
sourceFiles = map (flip (,) [])

data StaticLibrary = StaticLibrary {
    name :: String
  , sources :: CBuildFlags -> FilePath -> Rules (CBuildFlags, [SourceTree CBuildFlags])
  }

-- files :: [FilePath] -> CBuildFlags -> FilePath -> Rules (CBuildFlags, [SourceTree CBuildFlags])
-- files xs build _ = return (build, [SourceTree id (sourceFiles xs)])

libName :: StaticLibrary -> String
libName = ("lib"++) . flip replaceExtension "a" . name

libBuildDir :: Env -> CTarget -> StaticLibrary -> FilePath
libBuildDir env target lib = buildDir env target </> name lib

libBuildPath :: Env -> CTarget -> StaticLibrary -> FilePath
libBuildPath env target lib = buildDir env target </> libName lib

staticLibrary :: Env -> CTarget -> CToolChain -> CBuildFlags -> StaticLibrary -> Rules ()
staticLibrary env target toolChain buildFlags lib = do
    let buildDir = libBuildDir env target lib
    (buildFlags', srcTrees) <- sources lib buildFlags buildDir
    objects <- forM srcTrees $ \(SourceTree mapBuildFlags srcTree) -> do
        let src = map fst srcTree
            dep = map snd srcTree
            obj = map (combine buildDir . makeRelative buildDir . (<.> "o")) src
        zipWithM_ ($) (zipWith (staticObject target toolChain (mapBuildFlags buildFlags')) src dep) obj
        return obj
    libBuildPath env target lib ?=> do
        linkC target
              toolChain
              (linkerFlags `appendL` ["-static"] $ buildFlags')
              (concat objects)

-- ====================================================================
-- Target and build settings

cToolChain_IOS_Simulator :: CToolChain
cToolChain_IOS_Simulator =
    platformPrefix ^= "/Developer/Platforms/iPhoneSimulator.platform"
  $ prefix ^= "Developer/usr"
  $ compiler ^= "clang"
  $ linker ^= "libtool"
  $ defaultCToolChain

cToolChain_MacOSX :: CToolChain
cToolChain_MacOSX =
    platformPrefix ^= "/Developer/Platforms/MacOSX.platform"
  $ prefix ^= "/usr"
  $ compiler ^= "clang"
  $ linker ^= "libtool"
  $ defaultCToolChain

cBuildFlags_IOS_Simulator :: CBuildFlags
cBuildFlags_IOS_Simulator =
    appendL defines [("__IPHONE_OS_VERSION_MIN_REQUIRED", Just "40200")]
  $ appendL preprocessorFlags [ "-isysroot", platformPrefix `combineL` "Developer/SDKs/iPhoneSimulator5.0.sdk" $ cToolChain_IOS_Simulator ]
  $ appendL compilerFlags (flags "-f" ["visibility=hidden", "visibility-inlines-hidden"]
                            ++ flag "-g" "dwarf-2")
  $ defaultCBuildFlags

cBuildFlags_MacOSX :: CBuildFlags
cBuildFlags_MacOSX =
    appendL compilerFlags (flags "-f" ["visibility=hidden", "visibility-inlines-hidden"]
                            ++ flag "-g" "dwarf-2")
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
            _             -> []
     ++ [ serdDir, sordDir, lilvDir ] )
  $ systemIncludes `appendL`
       ( [ "src" ]
      ++ [ boostDir
         , "external_libraries/boost_lockfree" ] )
  $ buildFlags

mescalineLib :: CTarget -> IO StaticLibrary
mescalineLib target = do
    boostSrc <- find always
                    (extension ==? ".cpp" &&?
                     (not . isSuffixOf "win32") <$> directory &&?
                     (not . isSuffixOf "test/src") <$> directory &&?
                     (fileName /=? "utf8_codecvt_facet.cpp"))
                    boostDir
    return $ StaticLibrary "mescaline" $ \buildFlags _ -> return (buildFlags, [
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
                else [])
        ])

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
  , _targets :: [String]
  } deriving (Show)

defaultOptions :: Options
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
            buildFlags = cBuildFlags_IOS_Simulator
        libmescaline <- mescalineLib target
        shake $ do
            let libs = [ libmescaline ]
                lib = staticLibrary env target toolChain buildFlags
                libFile = libBuildPath env target
            mapM_ lib libs
            want (map libFile libs)
    )
  , ( "macosx",
    \shake env -> do
        let target = mkCTarget MacOSX "x86_64"
            toolChain = cToolChain_MacOSX
            buildFlags = cBuildFlags_MacOSX
        libmescaline <- mescalineLib target
        shake $ do
            let libs = [ libmescaline ]
                lib = staticLibrary env target toolChain buildFlags
                libFile = libBuildPath env target
            mapM_ lib libs
            want (map libFile libs)
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
    opts <- processArgs arguments
    let shakeIt = shake (optionsToShake opts)
        env = mkEnv (output ^$ opts)
    if help ^$ opts
        then print $ helpText [] HelpFormatDefault arguments
        else case targets ^$ opts of
                [] -> do
                    -- TODO: integrate this with option processing
                    putStrLn $ "Please chose a target:"
                    mapM_ (putStrLn . ("\t"++) . fst) targetSpecs
                ts -> processTargets shakeIt env ts
