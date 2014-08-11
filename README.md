# Methcla - Mobile sound engine

Methcla is a light-weight, efficient sound engine for mobile devices, see our [website](http://methc.la) for the full picture.

## Build requirements

* GCC >= 4.7 or Clang >= 3.3

## Building the sound engine

Our build system is written in Haskell using the [Shake](https://github.com/ndmitchell/shake) library and you need to install the latest [Haskell platform](http://www.haskell.org/platform/) before building Methcla.

First off, don't forget to

    git submodule update --init --recursive

after pulling from Methcla's repository.

Initialize the build system:

    ./shake .update

This step needs to be repeated each time the build script's package dependencies change.

To build a specific target (see below for a list of possible targets):

    ./shake TARGET

To clean everything

    ./shake clean

Use the `-j` flag for parallel builds:

    ./shake -j4 test

The build script automatically passes the number of core in your build machine, use `-j1` to force a sequential build.

The `-c` flag allows to select a build configuration (*release* or *debug*):

    ./shake -c release test

Display the list of Shake command line options:

    ./shake -h

All built files are put in the `build` directory.

Here's a non-exhaustive list of targets:

* `test`: Run the test suite; this should be first thing to do when porting to a new platform.
* `desktop`: Build a shared library for the host operating system
* `iphoneos`: Build a static library for iOS devices
* `iphone-universal`: Build a static library for iOS devices and simulator
* `android`: Build a static library for Android
* `pnacl`: Build a static library for Pepper/PNaCl

## Examples

[thADDeus](https://github.com/samplecount/methcla/tree/develop/engine/examples/thADDeus) is an example project that builds the engine for iOS and Android devices and provides a simple multitouch sine synthesizer.

The [sampler](https://github.com/samplecount/methcla/tree/develop/engine/examples/sampler) example is a simple multitouch sampler application.
