.PHONY: build test

build:
	cd build && ninja

test: build
	cd build && ctest -V
