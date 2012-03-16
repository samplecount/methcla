#!/bin/sh

boost_src="$1"
src_dirs="src platform external_libraries/boost_lockfree"
out_dir="./external_libraries/boost"

mkdir -p "$out_dir"
bcp --boost="$boost_src" --scan `find $src_dirs -name '*.hpp' -o -name '*.cpp'` "$out_dir"

rm -f "$out_dir/Jamroot"

rm -rf `find "$out_dir" -name doc -type d`
rm -rf `find "$out_dir" -name build -type d`

rm -rf "$out_dir/libs/filesystem/v2/example" \
       "$out_dir/libs/filesystem/v2/test" \
       "$out_dir/libs/filesystem/v3/example" \
       "$out_dir/libs/filesystem/v3/test"
