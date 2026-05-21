Status: ready-for-agent

# 05 — Plugin MODULE targets and soundfile API platform selection

## What to build

Clean up `plugins/CMakeLists.txt` so every Plugin is a proper `MODULE` target linked against the named INTERFACE targets from issue #02, with soundfile API Plugins selected per platform via CMake conditionals rather than implicit pkg-config availability.

Specific changes:
- Link each Plugin `PRIVATE` against the `oscpp` INTERFACE target (replaces raw include path)
- Always build: `sine`, `node_control`, `patch_cable`, `disksampler`, `sampler`, `soundfile_api_dummy`
- Soundfile API platform selection:
  - `APPLE`: build `soundfile_api_extaudiofile`, linking `CoreFoundation` and `AudioToolbox` via `find_library`
  - Linux: probe libsndfile via `pkg_check_modules(sndfile sndfile)`; if found, build `soundfile_api_libsndfile`
  - `soundfile_api_mpg123` stays optional on both platforms (probe via pkg-config)
- Set `LIBRARY_OUTPUT_DIRECTORY` to `${CMAKE_BINARY_DIR}/plugins` on all Plugin targets so the Engine can find them at test time
- Strip the `lib` prefix: `set_target_properties(... PREFIX "")`

## Acceptance criteria

- [ ] `cmake --build --preset debug` produces all always-built Plugin MODULE libraries in `build/debug/plugins/`
- [ ] On macOS, `soundfile_api_extaudiofile` is built; `soundfile_api_libsndfile` is not built unless explicitly enabled
- [ ] On Linux, `soundfile_api_libsndfile` is built when libsndfile is present; skipped with a status message when absent
- [ ] No Plugin target uses a raw `include_directories()` or hardcoded path to `external_libraries/`

## Blocked by

- `02-external-library-interface-targets.md`
- `04-methcla-target-structure-and-alias.md`
