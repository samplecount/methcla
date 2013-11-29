# Methcla - Mobile sound engine

Methcla is a light-weight, efficient sound engine for mobile devices, see our [website](http://methc.la) for the full picture.

## Build requirements

* GCC >= 4.7 or Clang >= 3.3

## Building the sound engine

Our build system is written in Haskell and you need to install at least the [Haskell platform](http://www.haskell.org/platform/) and [cabal-dev](http://hackage.haskell.org/package/cabal-dev) before building Methcla.

In the `engine` subdirectory execute

    ./shake update

initially and each time the build files change.

Note that for the examples bundled with methcla there's no need to call *shake* directly, that's taken care of by the respective platform specific build systems.

To build a specific target:

    ./shake TARGET

To clean everything

    ./shake clean

Use the `-j` flag for parallel builds:

    ./shake -j4 iphoneos

The `-c` flag allows to select a build configuration (*debug* is the default):

    ./shake -c release iphoneos

Display the list of commandline options:

    ./shake -h

## Examples

`examples/ios/Methcla/Methcla.xcodeproj` is an example XCode project that builds the engine for iOS devices and provides a simple multitouch sine synthesizer.
