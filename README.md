# Haskell client libraries and scripts

This directory contains various Haskell libraries and scripts that are used to
control the Mescaline audio engine.

### Building the sound engine

In the `engine` subdirectory execute

    ./shake configure

initially and each time the build files change.

Then, to build a specific target:

    ./shake build TARGET

To get a list of possible targets

    ./shake build


To clean everything

    ./shake clean

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
