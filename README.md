# Methcla - Mobile sound engine

Methcla is a light-weight, efficient sound engine for mobile devices, see our [website](http://methc.la) for the full picture.

## Build requirements

* GCC >= 4.7 or Clang >= 3.3

## Building the sound engine

Our build system is written in Haskell and you need to install at least the [Haskell platform](http://www.haskell.org/platform/) and [cabal-install 1.18](http://hackage.haskell.org/package/cabal-install) before building Methcla.

In the `engine` subdirectory execute

    ./stir update

initially and each time the build files change.

Note that for the examples bundled with methcla there's no need to call *stir* directly, that's taken care of by the respective platform specific build systems.

To build a specific target:

    ./stir TARGET

To clean everything

    ./stir clean

Use the `-j` flag for parallel builds:

    ./stir -j4 test

The `-c` flag allows to select a build configuration (*debug* is the default):

    ./stir -c release test

Display the list of commandline options:

    ./stir -h

## Examples

[thADDeus](https://github.com/samplecount/methcla/tree/develop/engine/examples/thADDeus) is an example project that builds the engine for iOS and Android devices and provides a simple multitouch sine synthesizer.

The [methcla-examples](https://github.com/samplecount/methcla-examples) repository currently contains a simple multitouch sampler application (it uses a streaming disk sampler for the Pro version of Methcla).
