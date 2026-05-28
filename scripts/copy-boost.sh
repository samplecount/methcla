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

set -e

if [ -z "$1" ]; then
    echo "Usage: $(basename "$0") BOOST_VERSION"
    echo "Example: $(basename "$0") 1.91.0"
    exit 1
fi

version="$1"
version_underscored=$(echo "$version" | tr '.' '_')
archive="boost_${version_underscored}.tar.bz2"
download_url="https://archives.boost.io/release/${version}/source/${archive}"
out_dir="$(cd "$(dirname "$0")/.." && pwd)/external_libraries/boost"
modules="heap lockfree container_hash"
namespace="methcla_boost"

tmpdir=$(mktemp -d)
trap 'rm -rf "$tmpdir"' EXIT

echo "Downloading Boost ${version} to ${tmpdir}..."
curl -L --fail -o "${tmpdir}/${archive}" "${download_url}"

echo "Extracting..."
tar -xjf "${tmpdir}/${archive}" -C "$tmpdir"

boost_src="${tmpdir}/boost_${version_underscored}"
bcp="${boost_src}/dist/bin/bcp"

echo "Building bcp..."
( cd "$boost_src" && ./bootstrap.sh --with-libraries='' && ./b2 tools/bcp ) || exit 1

echo "Running bcp --namespace ${namespace}..."
rm -rf "$out_dir"
mkdir -p "$out_dir"
"$bcp" --boost="$boost_src" --namespace="$namespace" $modules "$out_dir"

rm -rf "$out_dir/Jamroot"
find "$out_dir" -name doc -type d -exec rm -rf {} + 2>/dev/null || true
find "$out_dir/libs" -name build -type d -exec rm -rf {} + 2>/dev/null || true
find "$out_dir/libs" -name test -type d -exec rm -rf {} + 2>/dev/null || true
find "$out_dir/libs" -name example -type d -exec rm -rf {} + 2>/dev/null || true

echo "Done. Boost ${version} with namespace '${namespace}' installed to ${out_dir}"
