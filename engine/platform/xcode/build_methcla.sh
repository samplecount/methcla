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

if [ -f "$HOME/.profile" ]; then
    . "$HOME/.profile"
fi

if [ -f "$HOME/.bash_login" ]; then
    . "$HOME/.bash_login"
fi

case $ACTION in
    clean)
        TARGET=clean ;;
    *)
        if [ -z "$PLATFORM_NAME" ]; then
            echo "Missing PLATFORM_NAME environment variable"
            exit 1
        fi
        TARGET="$PLATFORM_NAME";;
esac

./stir -V -c $CONFIGURATION -j`sysctl -n hw.ncpu` $TARGET || exit $?

# Touch this to force relinking against libmethcla.a in Xcode
touch platform/xcode/methcla_init.c
