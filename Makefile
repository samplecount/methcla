.PHONY: build clean test

build:
	cd build && ninja

clean:
	cd build && ninja clean

test: build
	cd build && ctest -V
