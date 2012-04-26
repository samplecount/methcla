# Haskell client libraries and scripts

This directory contains various Haskell libraries and scripts that are used to
control the Mescaline audio engine.

## General notes on building

The most convenient way to build the libraries and executables is to use the
various sandboxing tools available for Haskell development.

You need the following tools; each can be installed with `cabal install`:

* `cabal-meta`
* `virthualenv`

Create a local Haskell sandbox in `$PWD/.virthualenv` with

    $ virthualenv

Now, before building the libraries and executables, activate the `virthualenv`
configuration:

    $ source .virthualenv/bin/activate

### Building the sound engine

Need to document this

### Building the client libraries and scripts

Change to the `client` directory and initialize the build environment with

    $ cabal-meta install

Then build the Haskell code with

    $ cabal install

Executables will be installed in `../.virthualenv/bin/`.

## Running the server sound engine

The server sound engine can be run with

    $ .virthualenv/bin/streamero-sound-engine -?

for a list of options.

The engine expects a Unix domain socket file to be passed on the command line;
if omitted, the program will start in interactive mode.
