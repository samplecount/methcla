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

boost_src="$1"
src_dirs="include platform src"
out_dir="./external_libraries/boost"

mkdir -p "$out_dir"
"$boost_src/dist/bin/bcp" --boost="$boost_src" --scan `find $src_dirs -name '*.hpp' -o -name '*.cpp'` "$out_dir"

rm -f "$out_dir/Jamroot"

rm -rf `find "$out_dir" -name doc -type d`
rm -rf `find "$out_dir" -name build -type d`

rm -rf "$out_dir/libs/filesystem/v2/example" \
       "$out_dir/libs/filesystem/v2/test" \
       "$out_dir/libs/filesystem/v3/example" \
       "$out_dir/libs/filesystem/v3/test"
