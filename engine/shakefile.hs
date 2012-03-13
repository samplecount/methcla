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

(?=>) :: FilePath -> (FilePath -> Action ()) -> Rules ()
f ?=> a = (==f) ?> a

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
                $  flag_ "-arch" (buildArch build)
                ++ flags "-I" (systemIncludes build)
                ++ flags_ "-iquote" (userIncludes build)
                ++ defineFlags build
                ++ preprocessorFlags build
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
        linkC toolChain (build' { linkerFlags = linkerFlags build ++ ["-static"]}) objects

cToolChain = CToolChain {
    platform = "iOS-Simulator"
  , platformPrefix = "/Developer/Platforms/iPhoneSimulator.platform"
  , prefix = "Developer/usr"
  , compiler = "clang"
  , linker = "libtool"
  }

cBuild = CBuild {
    buildArch = "i386"
  , buildPrefix = "./build"
  , systemIncludes = []
  , userIncludes = []
  , defines = []
  , preprocessorFlags = []
  , compilerFlags = flags "-f" ["visibility=hidden", "visibility-inlines-hidden"]
                 ++ flag "-g" "dwarf-2"
  , linkerFlags = []
  }

serdDir = "external_libraries/serd-0.5.0"

serdBuild = cBuild {
    userIncludes = userIncludes cBuild ++ [ serdDir, "external_libraries" ]
  }

serdLib = StaticLibrary "serd" $ files $ under (serdDir </> "src") [
    "env.c"
  , "error.c"
  , "node.c"
  , "reader.c"
  , "uri.c"
  , "writer.c"
  ]

serd = (serdBuild, serdLib)

sordDir = "external_libraries/sord-0.5.0"

sordBuild = cBuild {
    userIncludes = userIncludes cBuild ++ [ sordDir, sordDir </> "src", serdDir, "external_libraries" ]
  }

-- sordLib = StaticLibrary "sord" $ \build buildDir -> do
--     let transFiles = under (sordDir </> "src") [
--                         "sord.c"
--                      ,  "zix/tree.h"    
--                      ,  "zix/tree.c" ]
--         srcFiles = under (sordDir </> "src") [
--                         "syntax.c"
--                       , "zix/hash.c" ]
--     -- FIXME: This is dependent on rule execution order!
--     want [buildDir </> sordDir </> "src/zix/tree.h"]
--     srcFiles' <- mapM (sourceTransform (combine buildDir) "s/zix_tree_/sord_zix_tree_/g") transFiles
--     return ( build { userIncludes = userIncludes build ++ [buildDir </> sordDir </> "src"] }
--            , srcFiles ++ (filter ((== ".c").takeExtension) srcFiles') )

sordLib = StaticLibrary "sord" $ files $ under (sordDir </> "src") [
    "sord.c"
  , "syntax.c"
  , "zix/tree.c"
  , "zix/hash.c"
  ]

sord = (sordBuild, sordLib)

lilvDir = "external_libraries/lilv-0.5.0"

lilvBuild = cBuild {
  userIncludes = userIncludes cBuild ++ [ lilvDir, lilvDir </> "src", serdDir, sordDir, "external_libraries" ]
  }

lilvLib = StaticLibrary "lilv" $ files $ under (lilvDir </> "src") [
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

lilv = (lilvBuild, lilvLib)

boostDir = "external_libraries/boost"

boostBuild = cBuild {
    systemIncludes = systemIncludes cBuild
        ++ [ boostDir ]
  }

mkBoost = do
    src <- find always
             (extension ==? ".cpp" &&?
              (not . isSuffixOf "win32") <$> directory &&?
              (not . isSuffixOf "test/src") <$> directory &&?
              (fileName /=? "utf8_codecvt_facet.cpp"))
             boostDir
    return (boostBuild, StaticLibrary "boost" $ files src)

mescalineBuild = cBuild {
    userIncludes = userIncludes cBuild
                      ++ ["."]
                      ++ [ "external_libraries" ]
                      ++ [ "/usr/local/include" ] -- Gnargh
                      ++ [ lilvDir, serdDir, sordDir ]
  , systemIncludes = systemIncludes cBuild
      ++ [ "platform/ios", "src" ]
      ++ [ boostDir, "external_libraries/boost_lockfree" ]
  , defines = defines cBuild ++ [("__IPHONE_OS_VERSION_MIN_REQUIRED", Just "40200")]
  , preprocessorFlags = preprocessorFlags cBuild
      ++ [ "-isysroot", platformPrefix cToolChain </> "Developer/SDKs/iPhoneSimulator5.0.sdk" ]
}

mescalineLib = StaticLibrary "mescaline" $
        files $ under "src" [
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
     ++ under "platform/ios" [ "Mescaline/Audio/IO/RemoteIODriver.cpp" ]
     ++ [ "lv2/puesnada.es/plugins/sine.lv2/sine.cpp" ]

mescaline = (mescalineBuild, mescalineLib)

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
-- data Options = Sample {hello :: String} deriving (Show, Data, Typeable)

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
        "clean" -> removeDirectoryRecursive (buildPrefix build)
    processTargets shake build ts

main = do
    opts <- processArgs arguments
    let shakeIt = shake (optionsToShake opts)
    if help ^$ opts
        then print $ helpText [] HelpFormatDefault arguments
        else case targets ^$ opts of
                [] -> do
                    boost <- mkBoost
                    shakeIt $ do
                        let libs = [ serd, sord, lilv, boost, mescaline ]
                            lib = uncurry (staticLibrary cToolChain)
                            libFile = uncurry (libBuildPath cToolChain)
                        mapM_ lib libs
                        want (map libFile libs)
                ts -> processTargets shakeIt cBuild ts
