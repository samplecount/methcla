# Changelog

All notable changes to this project will be documented in this file.

The format is based on [Keep a Changelog](https://keepachangelog.com/en/1.1.0/),
and this project adheres to [Semantic Versioning](https://semver.org/spec/v2.0.0.html).

## [Unreleased]

### Added

- Playback rate control to disksampler
- Node placement options to node creation API commands (`Methcla::NodePlacement`)
- `methcla_world_synth_done` function to plugin API to notify the engine when processing is finished
- `operator bool` to `Methcla::NodeId`
- `Methcla::Engine::nodeEndedHandler` API method returning `/node/ended` notification handler
- `ExponentialFade` plugin (`METHCLA_PLUGINS_EXPONENTIAL_FADE_URI`) to `methcla_plugins_node_control` library
- Install and export CMake targets; `methcla::methcla` now available via `find_package` (#123)
- `METHCLA_BUILD_TESTS` CMake option, defaults to on when building as top-level project (#122)
- CMake presets (`debug`, `release`) replacing ad-hoc build configuration
- GitHub Actions CI replacing Travis CI
- `CONTRIBUTING.md` with build, test, and PR workflow instructions
- `docs/architecture.md` with Mermaid audio routing diagram
- `docs/osc-api.md` (moved from `doc/OSC API.md`)
- `examples/README.md` describing the thADDeus and sampler examples

### Removed

- PNaCl test harness (`tests/pnacl/`, `examples/common/`)
- Duplicate `tinydir` copy in `examples/sampler/libs/`; Xcode projects now reference `external_libraries/tinydir`

### Changed

- Disabled clang-format for JavaScript files
- Dependency management modernised: `FetchContent` for oscpp and googletest; Boost 1.91.0 vendored with `methcla_boost` namespace to avoid symbol collisions (#129)
- C++ standard raised to C++17
- `Driver::Options` channel and buffer size fields changed from `int` to `size_t` (#80)
- Plugin libraries now build as CMake `MODULE` targets; soundfile API plugin selected per platform (#121)
- CMake target structure cleaned up: proper `PUBLIC`/`PRIVATE` dependencies, `methcla::methcla` alias target (#120)
- Warning flags scoped per-target via `methcla_target_warnings()` (#120)
- README rewritten: corrects platform scope (macOS and Linux desktop), updates build instructions, adds usage snippet

### Fixed

- Bus zeroing logic (#102)
- Bug in linked list implementation when adding a node before or after an existing node
- Rounding, float precision, and type safety issues (#135)
- Implicit `NodeId` conversions broken by explicit constructor (#81)
- Missing `#include` directives causing build failures with GCC 13 and recent Clang
- GCC 13 false positive warnings from Boost lockfree and tinydir

### Removed

- `Methcla_Resource` from plugin API: removed argument from `Methcla_SynthDef::construct`; renamed `methcla_world_resource_retain`/`methcla_world_resource_release` to `methcla_world_synth_retain`/`methcla_world_synth_release`
- Dead platform code and unused vendored libraries (Android, iOS, PNaCl, NaCl) (#128)
- `VERSION` file (version is solely in `CMakeLists.txt`)
- Stale `doc/Notes.md` and `doc/diagrams/`

## [0.2.0]

### Added

- Function for querying library version (`methcla_version`, `Methcla::version()`)
- Function for changing debug logging behaviour (`methcla_engine_set_log_flags`, `Methcla::Engine::setLogFlags`)
- ExtAudioFile soundfile API (macOS)

### Changed

- Split synth creation into `/synth/new` (`Methcla::Engine::synth`) and `/synth/activate` (`Methcla::Engine::activate`)
- Refactored engine interface: renamed `Methcla::Engine::Request` to `Methcla::Request`, removed `Methcla::Engine::Bundle`; `Methcla::Request` now has `openBundle`/`closeBundle` for nested bundle structures
- Renamed `Methcla::Engine::freeNode` to `Methcla::Engine::free`
- Moved plugin includes to `<methcla/plugins/*>`

[unreleased]: https://github.com/samplecount/methcla/compare/v0.2.0...HEAD
[0.2.0]: https://github.com/samplecount/methcla/releases/tag/v0.2.0
