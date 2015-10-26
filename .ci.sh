#!/bin/sh -e

if [ -z "$target" ]; then
    echo "\$target environment variable is not set"
    exit 1
fi

stack setup
stack build

libSegFault=/lib/x86_64-linux-gnu/libSegFault.so
if [ -f "${libSegFault}" ]; then
  export LD_PRELOAD="${libSegFault}"
fi

stack exec -- methcla-shakefile "$target"
