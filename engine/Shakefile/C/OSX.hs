module Shakefile.C.OSX where

import           Control.Applicative ((<$>))
import           Control.Lens hiding ((<.>))
import           Development.Shake as Shake
import           Development.Shake.FilePath
import           Data.List (intercalate)
import           Data.List.Split (splitOn)
import           Data.Version (Version(..), showVersion)
import           Shakefile.C
import           Shakefile.Lens (append, prepend)
import           System.Process (readProcess)

osxArchiver :: Archiver
osxArchiver toolChain buildFlags inputs output = do
    need inputs
    system' (tool archiverCmd toolChain)
          $  buildFlags ^. archiverFlags
          ++ ["-static"]
          ++ ["-o", output]
          ++ inputs

archFlags :: Target -> [String]
archFlags target = ["-arch", (archString $ target ^. targetArch)]

osxLinker :: LinkResult -> Linker
osxLinker link toolChain =
    case link of
        Executable     -> defaultLinker toolChain
        SharedLibrary  -> defaultLinker toolChain . prepend linkerFlags ["-dynamiclib"]
        DynamicLibrary -> defaultLinker toolChain . prepend linkerFlags ["-bundle"]

newtype DeveloperPath = DeveloperPath { developerPath :: FilePath }

-- | Get base path of development tools on OSX.
getDeveloperPath :: IO DeveloperPath
getDeveloperPath =
  (DeveloperPath . head . splitOn "\n")
    <$> readProcess "xcode-select" ["--print-path"] ""

platformDeveloperPath :: DeveloperPath -> String -> FilePath
platformDeveloperPath developer platform =
  developerPath developer </> "Platforms" </> (platform ++ ".platform") </> "Developer"

macOSX :: Version -> Platform
macOSX = Platform "MacOSX"

iPhoneOS :: Version -> Platform
iPhoneOS = Platform "iPhoneOS"

iPhoneSimulator :: Version -> Platform
iPhoneSimulator = Platform "iPhoneSimulator"

target :: Arch -> Platform -> Target
target arch = mkTarget arch "apple" "darwin10"

platformSDKPath :: DeveloperPath -> Platform -> FilePath
platformSDKPath developer platform =
      platformDeveloperPath developer name
  </> "SDKs"
  </> (name ++ showVersion (platformVersion platform) ++ ".sdk")
  where name = platformName platform

-- | Get OSX system version (first two digits).
getSystemVersion :: IO Version
getSystemVersion =
  flip Version []
    <$> (map read . take 2 . splitOn ".")
    <$> readProcess "sw_vers" ["-productVersion"] ""

osxLinkResultFileName :: LinkResult -> String -> FilePath
osxLinkResultFileName Executable = id
osxLinkResultFileName SharedLibrary = ("lib"++) . (<.> "dylib")
osxLinkResultFileName DynamicLibrary =            (<.> "dylib")

cToolChain_MacOSX :: DeveloperPath -> ToolChain
cToolChain_MacOSX developer =
    prefix .~ Just (developerPath developer </> "Toolchains/XcodeDefault.xctoolchain/usr")
  $ compilerCmd .~ "clang"
  $ archiverCmd .~ "libtool"
  $ archiver .~ osxArchiver
  $ linkerCmd .~ "clang++"
  $ linker .~ osxLinker
  $ linkResultFileName .~ osxLinkResultFileName 
  $ defaultToolChain

cToolChain_MacOSX_gcc :: DeveloperPath -> ToolChain
cToolChain_MacOSX_gcc developer =
    compilerCmd .~ "gcc"
  $ linkerCmd .~ "g++"
  $ cToolChain_MacOSX developer

osxDefaultBuildFlags :: Target -> DeveloperPath -> BuildFlags
osxDefaultBuildFlags target developer =
    append preprocessorFlags [ "-isysroot", sysRoot ]
  . append compilerFlags [(Nothing, archFlags target)]
  . append linkerFlags (archFlags target ++ [ "-isysroot", sysRoot ])
  $ defaultBuildFlags
  where sysRoot = platformSDKPath developer (target ^. targetPlatform)

cBuildFlags_MacOSX :: Target -> DeveloperPath -> BuildFlags
cBuildFlags_MacOSX target developer =
    append compilerFlags [(Nothing, ["-mmacosx-version-min=" ++ showVersion (platformVersion (target ^. targetPlatform))])]
  $ osxDefaultBuildFlags target developer

iosMinVersion :: String
iosMinVersion = "5.0" -- Required for C++11
--iosMinVersion = "40200"

cToolChain_IOS :: DeveloperPath -> ToolChain
cToolChain_IOS = cToolChain_MacOSX

cToolChain_IOS_gcc :: DeveloperPath -> ToolChain
cToolChain_IOS_gcc developer =
    prefix .~ Just (developerPath developer </> "Platforms/iPhoneOS.platform/Developer/usr")
  $ compilerCmd .~ "llvm-gcc"
  $ linkerCmd .~ "llvm-g++"
  $ cToolChain_IOS developer

cBuildFlags_IOS :: Target -> DeveloperPath -> BuildFlags
cBuildFlags_IOS target developer =
    append compilerFlags [(Nothing, ["-miphoneos-version-min=" ++ iosMinVersion])]
  $ osxDefaultBuildFlags target developer

cToolChain_IOS_Simulator :: DeveloperPath -> ToolChain
cToolChain_IOS_Simulator = cToolChain_MacOSX

cToolChain_IOS_Simulator_gcc :: DeveloperPath -> ToolChain
cToolChain_IOS_Simulator_gcc developer =
    prefix .~ Just (developerPath developer </> "Platforms/iPhoneSimulator.platform/Developer/usr")
  $ cToolChain_IOS_gcc developer

cBuildFlags_IOS_Simulator :: Target -> DeveloperPath -> BuildFlags
cBuildFlags_IOS_Simulator target developer =
    append compilerFlags [(Nothing, ["-miphoneos-version-min=" ++ iosMinVersion])]
  $ osxDefaultBuildFlags target developer

universalBinary :: [FilePath] -> FilePath -> Rules FilePath
universalBinary inputs output = do
    output ?=> \_ -> do
        need inputs
        system' "lipo" $ ["-create", "-output", output] ++ inputs
    return output
