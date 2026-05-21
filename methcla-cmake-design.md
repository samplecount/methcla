# Methcla — CMake Migration Design Document
*Version 0.1 — Initial CMake build system design (DS-CMAKE-01)*

---

## 1. Vision

Replace methcla's Shake/Haskell build system with a modern CMake build for macOS and Linux desktop targets. The new build system eliminates the Haskell toolchain as a build prerequisite, integrates all dependencies via FetchContent or vendored headers, exposes a single `methcla::methcla` imported target consumable via both `add_subdirectory` and `find_package`, and integrates the test suite with CTest.

---

## 2. Vocabulary

**FetchContent** — CMake's built-in module (available since 3.11, `MakeAvailable` since 3.14) that clones or downloads a dependency's source at configure time and makes its CMake targets available in the same build graph. The primary dependency integration mechanism in this build.

**INTERFACE target** — A CMake library target with no compiled sources. Used for header-only dependencies; propagates include directories and compile options to consumers via `target_link_libraries`.

**Vendored headers** — Source files committed directly into the repository rather than fetched at build time. Used for the Boost header subset, preserving the existing `external_libraries/boost` approach.

**Imported target** — A CMake target representing a pre-built or externally defined library. `methcla::methcla` is an exported imported target that downstream consumers link against.

**`CMakePresets.json`** — A CMake-native configuration file (CMake 3.21+) that defines named build presets, replacing the `.cfg` files from the Shake build.

---

## 3. Dependency Integration

### 3.1 Header-only and source-included dependencies

**oscpp** (`kaoskorobase/oscpp`) is a header-only C++11 library with no build step. Integrated via FetchContent and exposed as a CMake `INTERFACE` target with its include directory propagated.

**tlsf** (`mattconte/tlsf`) is a two-file public domain C allocator. Integrated via FetchContent; its `.c` source is compiled into a thin internal `STATIC` library target and linked privately into methcla.

**Boost headers** (`lockfree`, `heap`, and transitive dependencies) are vendored directly in `external_libraries/boost`, preserving the current approach. Exposed as a CMake `INTERFACE` target pointing at that directory. No FetchContent, no full Boost build.

### 3.2 Built dependencies

**libsndfile** is integrated via FetchContent and built from source. The following options are forced before `FetchContent_MakeAvailable` to produce a minimal, self-contained build:

```cmake
set(BUILD_TESTING    OFF CACHE BOOL "" FORCE)
set(ENABLE_EXTERNAL_LIBS OFF CACHE BOOL "" FORCE)
set(ENABLE_MPEG      OFF CACHE BOOL "" FORCE)
set(BUILD_PROGRAMS   OFF CACHE BOOL "" FORCE)
set(BUILD_EXAMPLES   OFF CACHE BOOL "" FORCE)
```

This disables ogg/vorbis/flac/opus/mp3 optional dependencies and all non-library build targets. The resulting `SndFile::sndfile` target is linked privately into methcla.

**Catch2 v3** (test-only) is integrated via FetchContent. Tests are registered with CTest using `catch_discover_tests()`. Catch2 is only fetched when `METHCLA_BUILD_TESTS` is `ON`.

### 3.3 Platform audio I/O

Platform audio dependencies are found via CMake's standard mechanisms, not fetched:

- **macOS:** `find_library(COREAUDIO CoreAudio)` and `find_library(AUDIOUNIT AudioUnit)`
- **Linux:** `find_package(ALSA REQUIRED)` (exposes `ALSA::ALSA`)

### 3.4 Dependency summary

| Dependency | Integration | Target |
|---|---|---|
| oscpp | FetchContent | `INTERFACE` |
| tlsf | FetchContent | internal `STATIC` |
| Boost headers | Vendored | `INTERFACE` |
| libsndfile | FetchContent (source build) | `SndFile::sndfile` (private) |
| Catch2 v3 | FetchContent (test-only) | `Catch2::Catch2WithMain` |
| CoreAudio/AudioUnit | `find_library` | platform, private |
| ALSA | `find_package` | `ALSA::ALSA`, private |

---

## 4. Target Design

### 4.1 Public target

A single exported target `methcla::methcla` is the entire public surface of the build. Downstream consumers link against it and get all required include paths and compile requirements propagated automatically.

```cmake
add_library(methcla ...)
add_library(methcla::methcla ALIAS methcla)

target_include_directories(methcla
    PUBLIC
        $<BUILD_INTERFACE:${CMAKE_CURRENT_SOURCE_DIR}/include>
        $<INSTALL_INTERFACE:include>
)
```

All third-party dependencies are linked `PRIVATE` — they are not exposed to consumers.

### 4.2 Dual consumption support

The target supports both consumption modes:

- **`add_subdirectory`:** The `methcla::methcla` alias is immediately available to the parent project.
- **`find_package`:** An install step exports the target to a `methclaConfig.cmake` file. Consumers call `find_package(methcla REQUIRED)` and link against `methcla::methcla`.

The install block uses CMake's standard `install(TARGETS ... EXPORT ...)` + `install(EXPORT ...)` pattern with `FILE_SET` for headers (CMake 3.21).

---

## 5. Build Configuration

### 5.1 CMake version

`cmake_minimum_required(VERSION 3.21)` — required for `FILE_SET` (clean header installation), `CMakePresets.json` full support, and correct export behavior.

### 5.2 Presets

`CMakePresets.json` replaces the Shake `.cfg` files. Two configure presets are defined:

- **`debug`** — maps to `CMAKE_BUILD_TYPE=Debug`; enables `-DDEBUG=1`, address sanitizer optional
- **`release`** — maps to `CMAKE_BUILD_TYPE=Release`; enables `-DNDEBUG`, full optimization

Custom compiler flags from the Shake build (`-fvisibility-inlines-hidden`, `-fstrict-aliasing`, `-Werror=return-type`, `-Wall`, `-Wextra`) move into `target_compile_options` on the `methcla` target with `PRIVATE` visibility.

### 5.3 Offline builds

FetchContent supports offline use via the standard `FETCHCONTENT_SOURCE_DIR_<UPPERCASED_DEP>` variable. Users with local clones set this variable at configure time to bypass network access. No special handling required in CMakeLists.txt.

---

## 6. Test Integration

Tests are built when `METHCLA_BUILD_TESTS=ON` (default `OFF` for library consumers). The test executable links against `methcla::methcla` and `Catch2::Catch2WithMain`. CTest integration uses `catch_discover_tests()` which auto-registers all `TEST_CASE` entries.

```bash
cmake --preset debug -DMETHCLA_BUILD_TESTS=ON
cmake --build build/debug
ctest --test-dir build/debug
```

---

## 7. Scope

### In scope

- macOS and Linux desktop targets
- Static library build of methcla
- All dependency integration as described in §3
- Single `methcla::methcla` exported target
- `add_subdirectory` and `find_package` consumption
- CTest integration for Catch2 test suite
- `CMakePresets.json` with debug and release presets
- Compiler flags from existing Shake build migrated to `target_compile_options`
- `METHCLA_BUILD_TESTS` option

### Explicitly deferred

- iOS and Android cross-compilation
- Shared library (`.dylib`/`.so`) build variant
- Boost dependency elimination (tracked separately; issue #27)
- Package manager integration (vcpkg, Conan)
- CI/CD pipeline configuration

---

## 8. Future Directions (Not Committed)

- Eliminate Boost dependency entirely by replacing `boost::noncopyable` (issue #27) and finding/replacing `lockfree` and `heap` with alternatives or a minimal Boost fetch
- Shared library build variant with proper symbol visibility
- iOS and Android cross-compilation via CMake toolchain files
- vcpkg or Conan manifest for system dependency management
- GitHub Actions CI using the CMake presets directly

---

## Changelog

**0.1 — Initial CMake build system design (DS-CMAKE-01)**
- Initial document. All decisions from DS-CMAKE-01 recorded.
