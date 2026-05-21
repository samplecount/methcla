Status: ready-for-agent

# 08 — Verify `add_subdirectory` and `find_package` consumption

## What to build

Write two minimal consumer projects that verify the two consumption modes work end-to-end. These can live under `tests/consumers/` and are not part of the main build — they are standalone projects run manually or in CI.

**Consumer A — `add_subdirectory`:**
A `CMakeLists.txt` that calls `add_subdirectory(../../..)` (pointing at the methcla root) and builds a small executable that includes `<methcla/engine.h>` and calls `methcla_version()`. Links against `methcla::methcla`.

**Consumer B — `find_package`:**
A `CMakeLists.txt` that calls `find_package(methcla REQUIRED)` and builds the same small executable. Requires running `cmake --install` from issue #07 first and passing `-DCMAKE_PREFIX_PATH=/tmp/methcla-install` at configure time.

Both consumers must build without the consumer adding any include paths or library paths manually.

## Acceptance criteria

- [ ] Consumer A configures and compiles using `add_subdirectory` with no extra include or link flags
- [ ] Consumer B configures and compiles after `cmake --install` using `find_package` with no extra include or link flags
- [ ] Both consumers link `methcla::methcla` and produce a working executable
- [ ] Neither consumer's `CMakeLists.txt` references any path inside `external_libraries/`

## Blocked by

- `07-install-and-export-targets.md`
