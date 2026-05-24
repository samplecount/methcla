[![CI](https://github.com/samplecount/methcla/actions/workflows/ci.yml/badge.svg?branch=develop)](https://github.com/samplecount/methcla/actions/workflows/ci.yml)

# Methcla

A real-time C++ audio engine library for macOS and Linux. Exposes a C API with C++ bindings for embedding in host applications.

## Requirements

- CMake 3.24+, C++17 (GCC or Clang)
- Optional: [RtAudio](https://www.music.mcgill.ca/~gary/rtaudio/) for audio I/O (`-DMETHCLA_ENABLE_RTAUDIO=ON`)
- Optional: [libsndfile](https://libsndfile.github.io/libsndfile/) for soundfile support on Linux

## Build

```sh
cmake --preset debug        # configure (or: release)
cmake --build build/debug   # build
ctest --test-dir build/debug --output-on-failure  # test
```

## Usage

```cpp
#include <methcla/engine.hpp>
#include <methcla/plugins/sine.h>

Methcla::EngineOptions options;
options.addLibrary(methcla_plugins_sine);

Methcla::Engine engine(options);
engine.start();

// Create a sine synth (freq=440 Hz, amp=0.5) on hardware output bus 0
Methcla::Request request(&engine);
request.openBundle();
auto synth = request.synth(METHCLA_PLUGINS_SINE_URI, engine.root(), {440.f, 0.5f});
request.activate(synth);
request.mapOutput(synth, 0, Methcla::AudioBusId(0), Methcla::kBusMappingExternal);
request.closeBundle();
request.send();

// Free the synth when done
Methcla::Request stop(&engine);
stop.openBundle();
stop.free(synth);
stop.closeBundle();
stop.send();
```

See [`docs/usage.md`](docs/usage.md) for a full usage guide, [`docs/osc-api.md`](docs/osc-api.md) for the full command protocol, and [`docs/architecture.md`](docs/architecture.md) for an overview of audio routing.

## Integration

**add_subdirectory:**

```cmake
add_subdirectory(path/to/methcla)
target_link_libraries(myapp PRIVATE methcla::methcla)
```

**FetchContent:**

```cmake
include(FetchContent)
FetchContent_Declare(methcla
    GIT_REPOSITORY https://github.com/samplecount/methcla.git
    GIT_TAG        develop)
FetchContent_MakeAvailable(methcla)
target_link_libraries(myapp PRIVATE methcla::methcla)
```

**find_package** (after `cmake --install`):

```cmake
find_package(methcla REQUIRED)
target_link_libraries(myapp PRIVATE methcla::methcla)
```

## Examples

See [`examples/`](examples/) for sample applications.

## Contributing

See [`CONTRIBUTING.md`](CONTRIBUTING.md).
