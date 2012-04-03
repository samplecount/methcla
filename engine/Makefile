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
