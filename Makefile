.PHONY: build clean test

build: build/CMakeCache.txt
	cd build && ninja

build/CMakeCache.txt:
	mkdir -p build
	cd build && cmake -G Ninja ..

clean:
	cd build && ninja clean

test: build
	cd build && ctest -V
