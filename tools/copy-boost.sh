#!/bin/sh
# Copyright 2012-2013 Samplecount S.L.
# 
# Licensed under the Apache License, Version 2.0 (the "License");
# you may not use this file except in compliance with the License.
# You may obtain a copy of the License at
# 
#     http://www.apache.org/licenses/LICENSE-2.0
# 
# Unless required by applicable law or agreed to in writing, software
# distributed under the License is distributed on an "AS IS" BASIS,
# WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
# See the License for the specific language governing permissions and
# limitations under the License.

if [ -z "$1" ]; then
    echo "Usage: `basename $0` BOOST_SOURCE_DIR [OUT_DIR MODULES...]"
    exit 1
fi

boost_src="$1" ; shift

if [ -z "$1" ]; then
  out_dir="./external_libraries/boost"
else
  out_dir="$1" ; shift
fi

if [ -z "$@" ]; then
  # src_dirs="include platform plugins src tests"
  modules="atomic assert heap lockfree serialization utility"
else
  # src_dirs="$@"
  modules="$@"
fi

bcp="${boost_src}/dist/bin/bcp"

if [ ! -x "$bcp" ]; then
    ( cd "$boost_src" && ./bootstrap.sh && ./b2 tools/bcp ) || exit 1
fi

mkdir -p "$out_dir"
# "$bcp" --boost="$boost_src" --scan `find $src_dirs -name '*.h*' -o -name '*.cpp'` "$out_dir"
"$bcp" --boost="$boost_src" $modules "$out_dir"

rm -rf "$out_dir/Jamroot"
rm -rf `find "$out_dir" -name doc -type d`
rm -rf `find "$out_dir/libs" -name build -type d`
rm -rf `find "$out_dir/libs" -name test -type d`
rm -rf `find "$out_dir/libs" -name example -type d`
