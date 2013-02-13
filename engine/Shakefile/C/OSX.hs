module Shakefile.C.OSX where

import           Control.Applicative ((<$>))
import           Control.Lens
import           Development.Shake as Shake
import           Development.Shake.FilePath
import           Data.List (intercalate)
import           Data.List.Split (splitOn)
import           Shakefile.C
import           System.Process (readProcess)

-- ====================================================================
-- Target and build defaults

osxArchiver :: Archiver
osxArchiver _ toolChain buildFlags inputs output = do
    need inputs
    systemLoud (tool archiverCmd toolChain)
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

platformSDKPath :: DeveloperPath -> String -> String -> FilePath
platformSDKPath developer platform sdkVersion =
  platformDeveloperPath developer platform </> "SDKs" </> (platform ++ sdkVersion ++ ".sdk")

-- | Get OSX system version (first two digits).
getSystemVersion :: IO String
getSystemVersion =
  (intercalate "." . take 2 . splitOn ".")
    <$> readProcess "sw_vers" ["-productVersion"] ""

cToolChain_MacOSX :: DeveloperPath -> CToolChain
cToolChain_MacOSX developer =
    prefix .~ Just (developerPath developer </> "Toolchains/XcodeDefault.xctoolchain/usr")
  $ compilerCmd .~ "clang"
  $ archiverCmd .~ "libtool"
  $ archiver .~ osxArchiver
  $ linkerCmd .~ "clang++"
  $ linker .~ osxLinker
  $ defaultCToolChain

cToolChain_MacOSX_gcc :: DeveloperPath -> CToolChain
cToolChain_MacOSX_gcc developer =
    compilerCmd .~ "gcc"
  $ linkerCmd .~ "g++"
  $ cToolChain_MacOSX developer

cBuildFlags_MacOSX :: DeveloperPath -> String -> CBuildFlags
cBuildFlags_MacOSX developer sdkVersion =
    append preprocessorFlags [
      "-isysroot"
    , platformSDKPath developer "MacOSX" sdkVersion ]
  . append compilerFlags [(Nothing, flag ("-mmacosx-version-min=" ++ sdkVersion))]
  $ defaultCBuildFlags

iosMinVersion :: String
iosMinVersion = "50000"
--iosMinVersion = "40200"

cToolChain_IOS :: DeveloperPath -> CToolChain
cToolChain_IOS = cToolChain_MacOSX

cBuildFlags_IOS :: DeveloperPath -> String -> CBuildFlags
cBuildFlags_IOS developer sdkVersion =
    append defines [("__IPHONE_OS_VERSION_MIN_REQUIRED", Just iosMinVersion)]
  . append preprocessorFlags
            [ "-isysroot"
            , platformSDKPath developer "iPhoneOS" sdkVersion ]
  $ defaultCBuildFlags

cToolChain_IOS_Simulator :: DeveloperPath -> CToolChain
cToolChain_IOS_Simulator = cToolChain_MacOSX

cBuildFlags_IOS_Simulator :: DeveloperPath -> String -> CBuildFlags
cBuildFlags_IOS_Simulator developer sdkVersion =
    append defines [("__IPHONE_OS_VERSION_MIN_REQUIRED", Just iosMinVersion)]
  . append preprocessorFlags
            [ "-isysroot"
            , platformSDKPath developer "iPhoneSimulator" sdkVersion ]
  $ defaultCBuildFlags

