.PHONY: all clean shake-dependencies

NCPU := $(shell sysctl -n hw.ncpu)
DEFAULT_TARGET := macosx

all:
	./shake -j$(NCPU) macosx

clean:
	./shake clean

shake-dependencies:
	cabal install cmdargs data-lens-template filemanip shake
