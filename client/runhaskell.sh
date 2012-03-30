#!/bin/sh

base=`dirname $0`
export MESCALINE_LV2_PATH="$base/../engine/lv2/bundles"

runhaskell -L"$base"/../engine/build/debug/macosx/x86_64 -lmescaline "$@"
