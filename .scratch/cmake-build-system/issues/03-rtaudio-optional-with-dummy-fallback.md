Status: ready-for-agent

# 03 — RtAudio optional with DummyDriver fallback

## What to build

Make the RtAudio Driver conditional on `METHCLA_ENABLE_RTAUDIO` (default `ON`). When OFF, the Engine links the DummyDriver instead. This allows headless and offline builds without a system audio daemon.

Specific changes:
- In root `CMakeLists.txt`, gate `add_subdirectory(platform)` content on `METHCLA_ENABLE_RTAUDIO` (option already added in issue #01)
- In `platform/rtaudio/CMakeLists.txt`, change `pkg_check_modules(rtaudio rtaudio REQUIRED)` to a non-required probe; if not found and `METHCLA_ENABLE_RTAUDIO=ON`, emit a fatal error with a clear message; if `METHCLA_ENABLE_RTAUDIO=OFF`, skip the target entirely
- Add a `driver_dummy` target in `platform/` (or `src/`) that compiles `src/Methcla/Audio/IO/DummyDriver.cpp`
- In `src/CMakeLists.txt`, link either `driver_rtaudio` or `driver_dummy` into `methcla` depending on the option

## Acceptance criteria

- [ ] `cmake --preset debug` with default options finds RtAudio and links `driver_rtaudio`
- [ ] `cmake --preset debug -DMETHCLA_ENABLE_RTAUDIO=OFF` configures and builds without requiring RtAudio
- [ ] With `METHCLA_ENABLE_RTAUDIO=OFF`, the Engine initialises using DummyDriver (verified by running one of the test executables)
- [ ] Missing RtAudio with `METHCLA_ENABLE_RTAUDIO=ON` produces a clear fatal error, not a cryptic link failure

## Blocked by

- `01-cmake-project-scaffold-and-presets.md`
