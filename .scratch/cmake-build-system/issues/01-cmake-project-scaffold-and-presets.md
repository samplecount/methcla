Status: done

# 01 — CMake project scaffold and presets

## What to build

Raise the CMake minimum to 3.21, add `CMakePresets.json` with debug and release presets, and set the macOS deployment target. This is the foundation everything else builds on — when this slice is done, `cmake --preset debug` on a clean checkout configures without errors.

Specific changes:
- `cmake_minimum_required(VERSION 3.21)` in root `CMakeLists.txt`
- Set `CMAKE_OSX_DEPLOYMENT_TARGET` to `16` before the `project()` call
- Add `METHCLA_ENABLE_RTAUDIO` option (default `ON`) and `METHCLA_BUILD_TESTS` option (default `OFF`) at the root level
- Create `CMakePresets.json` at the repo root with two configure presets (`debug` → `CMAKE_BUILD_TYPE=Debug`, `release` → `CMAKE_BUILD_TYPE=Release`), both setting `CMAKE_OSX_DEPLOYMENT_TARGET=16`; add matching build presets that inherit from each

## Acceptance criteria

- [ ] `cmake --preset debug` completes without errors on macOS
- [ ] `cmake --preset release` completes without errors on macOS
- [ ] `cmake --preset debug` completes without errors on Linux
- [ ] `CMAKE_OSX_DEPLOYMENT_TARGET` is `16` in the configure output on macOS
- [ ] `METHCLA_ENABLE_RTAUDIO` and `METHCLA_BUILD_TESTS` appear in `cmake -L` output with correct defaults

## Blocked by

None — can start immediately.
