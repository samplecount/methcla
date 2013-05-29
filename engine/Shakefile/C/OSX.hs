module Shakefile.C.OSX where

import           Control.Applicative ((<$>))
import           Control.Lens hiding ((<.>))
import           Development.Shake as Shake
import           Development.Shake.FilePath
import           Data.List (intercalate)
import           Data.List.Split (splitOn)
import           Shakefile.C
import           Shakefile.Lens (append, prepend)
import           System.Process (readProcess)

osxArchiver :: Archiver
osxArchiver _ toolChain buildFlags inputs output = do
    need inputs
    system' (tool archiverCmd toolChain)
          $  buildFlags ^. archiverFlags
          ++ flag "-static"
          ++ flag_ "-o" output
          ++ inputs

osxLinker :: LinkResult -> Linker
osxLinker link target toolChain =
    case link of
        Executable     -> defaultLinker target toolChain
        SharedLibrary  -> defaultLinker target toolChain . prepend linkerFlags (flag "-dynamiclib")
        DynamicLibrary -> defaultLinker target toolChain . prepend linkerFlags (flag "-bundle")

newtype DeveloperPath = DeveloperPath { developerPath :: FilePath }

-- | Get base path of development tools on OSX.
getDeveloperPath :: IO DeveloperPath
getDeveloperPath =
  (DeveloperPath . head . splitOn "\n")
    <$> readProcess "xcode-select" ["--print-path"] ""

platformDeveloperPath :: DeveloperPath -> String -> FilePath
platformDeveloperPath developer platform =
  developerPath developer </> "Platforms" </> (platform ++ ".platform") </> "Developer"

macOSX :: Platform
macOSX = Platform "MacOSX"

iPhoneOS :: Platform
iPhoneOS = Platform "iPhoneOS"

iPhoneSimulator :: Platform
iPhoneSimulator = Platform "iPhoneSimulator"

newtype SDKVersion = SDKVersion { sdkVersionString :: String } deriving (Show)

data SDK = SDK Platform SDKVersion deriving (Show)

platformSDKPath :: DeveloperPath -> SDK -> FilePath
platformSDKPath developer (SDK (Platform platform) (SDKVersion sdkVersion)) =
  platformDeveloperPath developer platform </> "SDKs" </> (platform ++ sdkVersion ++ ".sdk")

-- | Get OSX system version (first two digits).
getSystemVersion :: IO SDKVersion
getSystemVersion =
  SDKVersion
    <$> (intercalate "." . take 2 . splitOn ".")
    <$> readProcess "sw_vers" ["-productVersion"] ""

osxLinkResultFileName :: LinkResult -> String -> FilePath
osxLinkResultFileName Executable = id
osxLinkResultFileName SharedLibrary = ("lib"++) . (<.> "dylib")
osxLinkResultFileName DynamicLibrary =            (<.> "dylib")

cToolChain_MacOSX :: DeveloperPath -> CToolChain
cToolChain_MacOSX developer =
    prefix .~ Just (developerPath developer </> "Toolchains/XcodeDefault.xctoolchain/usr")
  $ compilerCmd .~ "clang"
  $ archiverCmd .~ "libtool"
  $ archiver .~ osxArchiver
  $ linkerCmd .~ "clang++"
  $ linker .~ osxLinker
  $ linkResultFileName .~ osxLinkResultFileName 
  $ defaultCToolChain

cToolChain_MacOSX_gcc :: DeveloperPath -> CToolChain
cToolChain_MacOSX_gcc developer =
    compilerCmd .~ "gcc"
  $ linkerCmd .~ "g++"
  $ cToolChain_MacOSX developer

osxDefaultCBuildFlags :: DeveloperPath -> SDK -> CBuildFlags
osxDefaultCBuildFlags developer sdk =
    append preprocessorFlags [ "-isysroot", sysRoot ]
  . append linkerFlags [ "-isysroot", sysRoot ]
  $ defaultCBuildFlags
  where sysRoot = platformSDKPath developer sdk

cBuildFlags_MacOSX :: DeveloperPath -> SDKVersion -> CBuildFlags
cBuildFlags_MacOSX developer sdkVersion =
    append compilerFlags [(Nothing, flag ("-mmacosx-version-min=" ++ sdkVersionString sdkVersion))]
  $ osxDefaultCBuildFlags developer (SDK macOSX sdkVersion)

iosMinVersion :: String
iosMinVersion = "50000" -- Required for C++11
--iosMinVersion = "40200"

cToolChain_IOS :: DeveloperPath -> CToolChain
cToolChain_IOS = cToolChain_MacOSX

cToolChain_IOS_gcc :: DeveloperPath -> CToolChain
cToolChain_IOS_gcc developer =
    prefix .~ Just (developerPath developer </> "Platforms/iPhoneOS.platform/Developer/usr")
  $ compilerCmd .~ "llvm-gcc"
  $ linkerCmd .~ "llvm-g++"
  $ cToolChain_IOS developer

cBuildFlags_IOS :: DeveloperPath -> SDKVersion -> CBuildFlags
cBuildFlags_IOS developer sdkVersion =
    append defines [("__IPHONE_OS_VERSION_MIN_REQUIRED", Just iosMinVersion)]
  $ osxDefaultCBuildFlags developer (SDK iPhoneOS sdkVersion)

cToolChain_IOS_Simulator :: DeveloperPath -> CToolChain
cToolChain_IOS_Simulator = cToolChain_MacOSX

cToolChain_IOS_Simulator_gcc :: DeveloperPath -> CToolChain
cToolChain_IOS_Simulator_gcc developer =
    prefix .~ Just (developerPath developer </> "Platforms/iPhoneSimulator.platform/Developer/usr")
  $ cToolChain_IOS_gcc developer

cBuildFlags_IOS_Simulator :: DeveloperPath -> SDKVersion -> CBuildFlags
cBuildFlags_IOS_Simulator developer sdkVersion =
    append defines [("__IPHONE_OS_VERSION_MIN_REQUIRED", Just iosMinVersion)]
  $ osxDefaultCBuildFlags developer (SDK iPhoneSimulator sdkVersion)

universalBinary :: [FilePath] -> FilePath -> Rules FilePath
universalBinary inputs output = do
    output ?=> \_ -> do
        need inputs
        system' "lipo" $ ["-create", "-output", output] ++ inputs
    return output

