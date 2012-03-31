#!/bin/sh

base=`dirname $0`
export MESCALINE_LV2_PATH="$base/../engine/lv2/bundles"

if [ -n "$RELEASE" ]; then
    config=release
else
    config=debug
fi

echo "Using $config configuration ..."

runhaskell -L"$base"/../engine/build/$config/macosx/x86_64 -lmescaline "$@"
