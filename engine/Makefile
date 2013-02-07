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

.PHONY: all clean debug release shake-dependencies

NCPU := $(shell sysctl -n hw.ncpu)
TARGET := macosx
SHAKE_FLAGS = -j$(NCPU)
SHAKE = ./shake $(SHAKE_FLAGS)

all: debug

debug:
	$(SHAKE) -c debug $(TARGET)

release:
	$(SHAKE) -c release $(TARGET)
clean:
	$(SHAKE) clean

shake-dependencies:
	cabal install cmdargs data-lens-template filemanip shake

.PHONY: tags
tags:
	$(SHAKE) tags
