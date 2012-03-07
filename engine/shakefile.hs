import Control.Applicative ((<$>))
import Control.Monad
import Development.Shake
import Development.Shake.FilePath
import Data.List (intersperse)
import Data.Maybe
import GHC.Conc (numCapabilities)
import System.Environment
import Data.List.Split

under :: FilePath -> [FilePath] -> [FilePath]
under dir = map prepend
    where prepend ""   = dir
          prepend "."  = dir
          prepend ".." = takeDirectory dir
          prepend x    = combine dir x

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

buildDir :: CToolChain -> CBuild -> FilePath
buildDir toolChain build = buildPrefix build </> platform toolChain </> buildArch build

flag_ :: String -> String -> [String]
flag_ o x = [o, x]

flags_ :: String -> [String] -> [String]
flags_ o = concat . map (flag_ o)

flag :: String -> String -> [String]
flag f x = [f++x]

flags :: String -> [String] -> [String]
flags f = map (f++)

defineFlags :: CBuild -> [String]
defineFlags = flags "-D" . map (\(a, b) -> maybe a (\b -> a++"="++b) b) . defines

dependencyFile :: CToolChain -> CBuild -> FilePath -> FilePath -> Rules ()
dependencyFile toolChain build input output = do
    (==output) ?> \_ -> do
        need [input]
        system' (tool compiler toolChain)
                $  flag_ "-arch" (buildArch build)
                ++ flags "-I" (systemIncludes build)
                ++ flags_ "-iquote" (userIncludes build)
                ++ defineFlags build
                ++ preprocessorFlags build
                ++ ["-M", "-o", output, input]

dependencies :: String -> [FilePath]
dependencies = map (normalise.rstrip) . filter (\s -> not (null s) && s /= "\\\n") . drop 2 . splitOn " "
    where rstrip = reverse . dropWhile (=='\n') . reverse

staticObject :: CToolChain -> CBuild -> FilePath -> FilePath -> Rules ()
staticObject toolChain build input output = do
    let dep = replaceExtension output "d"
    dependencyFile toolChain build input dep
    (==output) ?> \_ -> do
        need [input, dep]
        deps <- dependencies <$> readFile' dep
        -- liftIO $ print deps
        need deps
        system' (tool compiler toolChain)
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
    system' (tool linker toolChain)
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
      ++ under "external_libraries" (concat [ [ "boost/boost", "boost_lockfree" ]
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
      , "Mescaline/Audio/Synth.cpp"
      , "Mescaline/Audio/SynthDef.cpp"
      , "Mescaline/Memory/Manager.cpp"
      , "Mescaline/Memory.cpp"
      ]
 ++ under "platform/ios" [ "Mescaline/Audio/IO/RemoteIODriver.cpp" ]
 ++ [ "lv2/puesnada.es/plugins/sine.lv2/sine.cpp" ]

getShakeOptions :: IO ShakeOptions
getShakeOptions = do
    nc <- return numCapabilities -- This has been changed to Control.Concurrent.getNumCapabilities in 7.?
    return $ shakeOptions {
        shakeVerbosity = Loud
      , shakeThreads = nc
      , shakeReport = Just "shakefile.html"
      }

main = do
    targets <- getArgs
    opts <- getShakeOptions
    print opts
    shake opts $ do
        staticLibrary cToolChain serdBuild serd
        staticLibrary cToolChain sordBuild sord
        staticLibrary cToolChain lilvBuild lilv
        staticLibrary cToolChain mescalineBuild mescaline
        want [ libBuildPath cToolChain serdBuild serd
             , libBuildPath cToolChain sordBuild sord
             , libBuildPath cToolChain lilvBuild lilv
             , libBuildPath cToolChain mescalineBuild mescaline ]
