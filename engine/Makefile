.PHONY: all clean shake-dependencies

all:
	./shake -j2

clean:
	./shake clean

shake-dependencies:
	cabal install cmdargs data-lens-template filemanip shake
