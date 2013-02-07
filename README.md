# Methcla - Mobile sound engine

Methcla is a light-weight, efficient sound engine for mobile devices, see our [website](http://methc.la) for the full picture.

## Building the sound engine

Our build system is written in Haskell and you need to install at least the [Haskell platform](http://www.haskell.org/platform/) and [cabal-dev](http://hackage.haskell.org/package/cabal-dev) before building Methcla.

In the `engine` subdirectory execute

    ./shake update

initially and each time the build files change.

Then, to build a specific target:

    ./shake TARGET

To get a list of possible targets

    ./shake

To clean everything

    ./shake clean

Use the `-j` flag for parallel builds:

    ./shake -j4 ios

## Examples

is an example XCode project that builds the engine for iOS devices and plays back a simple sine synth.
