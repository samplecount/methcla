Status: ready-for-agent

# 04 — `methcla` target: PUBLIC/PRIVATE structure and `methcla::methcla` alias

## What to build

Clean up `src/CMakeLists.txt` so the `methcla` static library target has correct `PUBLIC`/`PRIVATE` visibility on all its dependencies, exposes the public include directory via generator expressions (so it works for both in-tree and installed consumers), and is aliased as `methcla::methcla`.

Specific changes:
- `target_include_directories(methcla PUBLIC $<BUILD_INTERFACE:${CMAKE_SOURCE_DIR}/include> $<INSTALL_INTERFACE:include>)`
- Link `oscpp`, `tlsf`, `boost_headers`, `tinydir` as `PRIVATE` (they are implementation details, not part of the public API)
- Link the Driver target (`driver_rtaudio` or `driver_dummy`) as `PRIVATE`
- On Linux, link `dl` and `pthread` as `PUBLIC` (consumers need them to use the library)
- Move compiler flags (`-fvisibility-inlines-hidden`, `-fstrict-aliasing`, `-Werror=return-type`, `-Wall`, `-Wextra`) from global scope into `target_compile_options(methcla PRIVATE ...)`
- Add `add_library(methcla::methcla ALIAS methcla)`

## Acceptance criteria

- [ ] `cmake --build --preset debug` produces `libmethcla.a` without errors
- [ ] `cmake --build --preset release` produces `libmethcla.a` without errors
- [ ] A minimal consumer target in the same tree that links `methcla::methcla` compiles without adding any include paths manually
- [ ] No `include_directories()` or `add_compile_options()` calls at directory scope in `src/CMakeLists.txt`

## Blocked by

- `02-external-library-interface-targets.md`
- `03-rtaudio-optional-with-dummy-fallback.md`
