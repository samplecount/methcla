#!/bin/sh -e

if [ -z "$target" ]; then
    echo "\$target environment variable is not set"
    exit 1
fi

stack setup
stack build
stack exec -- methcla-shakefile "$target"
