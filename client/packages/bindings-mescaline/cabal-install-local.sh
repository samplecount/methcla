#!/bin/sh -x

cabal install --extra-include-dirs=$PWD/../../../engine/platform/jack --extra-include-dirs=$PWD/../../../engine/external_libraries/lv2
