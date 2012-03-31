.PHONY: all clean debug release shake-dependencies

NCPU := $(shell sysctl -n hw.ncpu)
TARGET := macosx
SHAKE_FLAGS = -j$(NCPU)

all: debug

debug:
	./shake $(SHAKE_FLAGS) -c debug $(TARGET)

release:
	./shake $(SHAKE_FLAGS) -c release $(TARGET)
clean:
	./shake clean

shake-dependencies:
	cabal install cmdargs data-lens-template filemanip shake
