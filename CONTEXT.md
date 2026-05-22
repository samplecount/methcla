# Methcla

A real-time C++ audio engine library for macOS and Linux desktop targets. Exposes a C API with C++ bindings for embedding in host applications.

## Language

**Engine**:
The core audio processing runtime. Instantiated by a host application, runs an audio thread, manages Synths on AudioBuses, and loads Plugins.
_Avoid_: audio server, sound engine, runtime

**Driver**:
A platform-specific audio I/O backend that feeds samples to and from the Engine. RtAudio is the desktop Driver; DummyDriver is used for headless or offline use.
_Avoid_: audio backend, audio device

**Plugin**:
A shared library (`.so`/`.dylib`) that registers one or more SynthDefs with the Engine at load time. Loaded either dynamically from a directory or registered statically via function pointer.
_Avoid_: extension, module (conflicts with CMake MODULE library type)

**SynthDef**:
A synthesizer definition registered by a Plugin. Describes the processing graph and parameters for one unit of audio computation.
_Avoid_: instrument, unit generator, UGen

**Synth**:
A running instance of a SynthDef inside the Engine, allocated on an AudioBus.
_Avoid_: voice, instance, node (overloaded with the Node base class)

**AudioBus**:
A multi-channel buffer used for audio routing between Synths inside the Engine.
_Avoid_: channel, buffer, bus (acceptable shorthand in code)

**Soundfile API**:
A Plugin category that provides file I/O capabilities to other Plugins (disksampler, sampler). Multiple soundfile API Plugins may be registered; the Engine selects by capability. Platform implementations: ExtAudioFile (macOS), libsndfile (Linux).
_Avoid_: audio file backend, file reader

## Build system

CMake 3.24+, C++17, C99. Presets: `debug` and `release` (`CMakePresets.json`).

### Dependencies

All declared in `cmake/dependencies.cmake` via `FetchContent`, included before any `add_subdirectory`.

| Dependency | Kind | How |
|---|---|---|
| **oscpp** | header-only OSC library; part of the public API | FetchContent (pinned git SHA); provides `oscpp::oscpp` target |
| **googletest v1.14** | test-only | FetchContent |
| **tlsf** | two-level segregated-fit allocator; merged directly into `libmethcla` | vendored in `external_libraries/tlsf/`; CMake `OBJECT` library |
| **tinydir** | header-only directory listing | vendored in `external_libraries/tinydir/`; CMake `INTERFACE` library |
| **Boost** (lockfree, heap, container\_hash, smart\_ptr) | header-only subset; namespaced as `methcla_boost` | vendored in `external_libraries/boost/`; re-extracted with `tools/copy-boost.sh <version>` |
| **RtAudio** | optional desktop audio Driver | system package via `pkg-config` (`-DMETHCLA_ENABLE_RTAUDIO=ON`) |
| **libsndfile** | Soundfile API Plugin on Linux | system package via `pkg-config` |
