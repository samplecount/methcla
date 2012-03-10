{-# LANGUAGE TemplateHaskell #-}
import Control.Applicative ((<$>))
import Control.Monad
import Development.Shake
import Development.Shake.FilePath
import Data.Lens.Common
import Data.Lens.Template
import Data.List (intersperse)
import Data.List.Split
import Data.Maybe
import GHC.Conc (numCapabilities)
import qualified System.Console.CmdArgs.Implicit as C
import           System.Console.CmdArgs.Explicit
import System.Environment

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

data CToolChain = CToolChain {
    platform :: String
  , platformPrefix :: FilePath
  , prefix :: FilePath
  , compiler :: String
  , linker :: String
  } deriving (Show)

tool :: (CToolChain -> String) -> CToolChain -> FilePath
tool f toolChain =  platformPrefix toolChain </> prefix toolChain </> "bin" </> f toolChain

data CBuild = CBuild {
    buildArch :: String
  , buildPrefix :: String
  , systemIncludes :: [FilePath]
  , userIncludes :: [FilePath]
  , defines :: [(String, Maybe String)]
  , preprocessorFlags :: [String]
  , compilerFlags :: [String]
  , linkerFlags :: [String]
  } deriving (Show)

defineFlags :: CBuild -> [String]
defineFlags = flags "-D" . map (\(a, b) -> maybe a (\b -> a++"="++b) b) . defines

buildDir :: CToolChain -> CBuild -> FilePath
buildDir toolChain build = buildPrefix build </> platform toolChain </> buildArch build

systemLoud :: FilePath -> [String] -> Action ()
systemLoud cmd args = do
    putNormal $ unwords $ [cmd] ++ args
    system' cmd args

dependencyFile :: CToolChain -> CBuild -> FilePath -> FilePath -> Rules ()
dependencyFile toolChain build input output = do
    (==output) ?> \_ -> do
        need [input]
        systemLoud (tool compiler toolChain)
                $  flag_ "-arch" (buildArch build)
                ++ flags "-I" (systemIncludes build)
                ++ flags_ "-iquote" (userIncludes build)
                ++ defineFlags build
                ++ preprocessorFlags build
                ++ ["-M", "-o", output, input]

parseDependencies :: String -> [FilePath]
parseDependencies = map (normalise.rstrip) . filter (\s -> not (null s) && s /= "\\\n") . drop 2 . splitOn " "
    where rstrip = reverse . dropWhile (=='\n') . reverse

staticObject :: CToolChain -> CBuild -> FilePath -> FilePath -> Rules ()
staticObject toolChain build input output = do
    let dep = replaceExtension output "d"
    dependencyFile toolChain build input dep
    (==output) ?> \_ -> do
        need [input, dep]
        deps <- parseDependencies <$> readFile' dep
        need deps
        systemLoud (tool compiler toolChain)
                $  flag_ "-arch" (buildArch build)
                ++ flags "-I" (systemIncludes build)
                ++ flags_ "-iquote" (userIncludes build)
                ++ defineFlags build
                ++ preprocessorFlags build
                ++ compilerFlags build
                ++ ["-c", "-o", output, input]

linkC :: CToolChain -> CBuild -> [FilePath] -> FilePath -> Action ()
linkC toolChain build inputs output = do
    need inputs
    systemLoud (tool linker toolChain)
          $  flag_ "-arch_only" (buildArch build)
          ++ linkerFlags build
          ++ flag_ "-o" output
          ++ inputs

data StaticLibrary = StaticLibrary {
    name :: String
  , sources :: [FilePath]
  } deriving (Show)

libName :: StaticLibrary -> String
libName = ("lib"++) . flip replaceExtension "a" . name

libBuildDir :: CToolChain -> CBuild -> StaticLibrary -> FilePath
libBuildDir toolChain build lib = buildDir toolChain build </> name lib

libBuildPath :: CToolChain -> CBuild -> StaticLibrary -> FilePath
libBuildPath toolChain build lib = buildDir toolChain build </> libName lib

staticLibrary :: CToolChain -> CBuild -> StaticLibrary -> Rules ()
staticLibrary toolChain build lib = do
    let objects = map (combine (libBuildDir toolChain build lib) . flip replaceExtension "o" . takeFileName) (sources lib)
    zipWithM_ (staticObject toolChain build) (sources lib) objects
    (== (libBuildPath toolChain build lib)) ?> do
        linkC toolChain (build { linkerFlags = linkerFlags build ++ ["-static"]}) objects

cToolChain = CToolChain {
    platform = "iOS-Simulator"
  , platformPrefix = "/Developer/Platforms/iPhoneSimulator.platform"
  , prefix = "Developer/usr"
  , compiler = "clang"
  , linker = "libtool"
  }

cBuild = CBuild {
    buildArch = "i386"
  , buildPrefix = "build"
  , systemIncludes = []
  , userIncludes = []
  , defines = []
  , preprocessorFlags = []
  , compilerFlags = flags "-f" ["visibility=hidden", "visibility-inlines-hidden"]
                 ++ flag "-g" "dwarf-2"
  , linkerFlags = []
  }

serdBuild = cBuild {
    userIncludes = userIncludes cBuild
                ++ under "external_libraries/lv2/serd-0.5.0" [ ".", "build" ]
  }

serd = StaticLibrary "serd" $ under "external_libraries/lv2/serd-0.5.0/src" [
    "env.c"
  , "error.c"
  , "node.c"
  , "reader.c"
  , "uri.c"
  , "writer.c"
  ]

sordBuild = cBuild {
    userIncludes = userIncludes cBuild
                ++ under "external_libraries/lv2"
                    (under "sord-0.5.0" [ ".", "build", "src" ] ++ [ "serd-0.5.0" ])
  }

sord = StaticLibrary "sord" $ under "external_libraries/lv2/sord-0.5.0/src" [
    "sord.c"
  , "syntax.c"
  , "zix/hash.c"
  , "zix/tree.c"
  ]

lilvBuild = cBuild {
  userIncludes = userIncludes cBuild
              ++ under "external_libraries/lv2"
                  (under "lilv-0.5.0" [ ".", "build", "src" ] ++ [ "serd-0.5.0", "sord-0.5.0" ])
}

lilv = StaticLibrary "lilv" $ under "external_libraries/lv2/lilv-0.5.0" [
    "src/collections.c"
  , "src/instance.c"
  , "src/node.c"
  , "src/plugin.c"
  , "src/pluginclass.c"
  , "src/port.c"
  , "src/query.c"
  , "src/scalepoint.c"
  , "src/ui.c"
  , "src/util.c"
  , "src/world.c"
  , "src/zix/tree.c"
  ]

mescalineBuild = cBuild {
    userIncludes = userIncludes cBuild
                      ++ ["."]
                      ++ [ "external_libraries" ]
                      ++ [ "/usr/local/include" ] -- Gnargh
  , systemIncludes = systemIncludes cBuild
      ++ [ "platform/ios", "src" ]
      ++ under "external_libraries" (concat [ [ "boost", "boost_lockfree" ]
                                             , under "lv2" ["lilv-0.5.0", "serd-0.5.0", "sord-0.5.0"] ])
  , defines = defines cBuild ++ [("__IPHONE_OS_VERSION_MIN_REQUIRED", Just "40200")]
  , preprocessorFlags = preprocessorFlags cBuild
      ++ [ "-isysroot", platformPrefix cToolChain </> "Developer/SDKs/iPhoneSimulator5.0.sdk" ]
}

mescaline = StaticLibrary "mescaline" $
    under "src" [
        "Mescaline/Audio/AudioBus.cpp"
      , "Mescaline/Audio/Engine.cpp"
      , "Mescaline/Audio/Group.cpp"
      , "Mescaline/Audio/Node.cpp"
      , "Mescaline/Audio/Resource.cpp"
      , "Mescaline/Audio/Synth.cpp"
      , "Mescaline/Audio/SynthDef.cpp"
      , "Mescaline/Memory/Manager.cpp"
      , "Mescaline/Memory.cpp"
      ]
 ++ under "platform/ios" [ "Mescaline/Audio/IO/RemoteIODriver.cpp" ]
 ++ [ "lv2/puesnada.es/plugins/sine.lv2/sine.cpp" ]

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
  , _targets :: [String]
  } deriving (Show)
-- data Options = Sample {hello :: String} deriving (Show, Data, Typeable)

defaultOptions = Options {
    _help = False
  , _verbosity = Normal
  , _jobs = 1
  , _output = "./build"
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
         ]
          -- ++ flagsVerbosity (setL verbosity)
    where upd what x = Right . setL what x

optionsToShake opts = shakeOptions {
    shakeThreads = jobs ^$ opts
  , shakeVerbosity = verbosity ^$ opts
  , shakeReport = Just $ (output ^$ opts) </> "report.html"
  }

-- options buildDir = o {
--     shakeFiles = shakeFiles o &= C.help "Where to put the shake files"
--   , shakeThreads = shakeThreads o &= C.help "Number of parallel jobs"
--   , shakeReport = Just (buildDir </> "report.html") &= C.help "Where to store build report"
--   } &= C.summary "Shake v1"
--   where o = shakeOptions

-- main = print =<< cmdArgs sample

main = do
    -- opts <- getShakeOptions (buildPrefix cBuild)
    -- opts <- C.cmdArgs (options (buildPrefix cBuild))
    -- targets <- getArgs
    -- print targets
    opts <- processArgs arguments
    if help ^$ opts
        then print $ helpText [] HelpFormatDefault arguments
        else 
            shake (optionsToShake opts) $ do
                staticLibrary cToolChain serdBuild serd
                staticLibrary cToolChain sordBuild sord
                staticLibrary cToolChain lilvBuild lilv
                staticLibrary cToolChain mescalineBuild mescaline
                want [ libBuildPath cToolChain serdBuild serd
                     , libBuildPath cToolChain sordBuild sord
                     , libBuildPath cToolChain lilvBuild lilv
                     , libBuildPath cToolChain mescalineBuild mescaline ]
