Status: ready-for-agent

# 02 — External library INTERFACE targets (oscpp, Boost, tinydir, tlsf)

## What to build

Rewrite `external_libraries/CMakeLists.txt` so every dependency exposes a named CMake target. Downstream targets (`methcla`, plugins) link against these targets instead of reaching into directory paths directly. All four dependencies remain as git submodules — no FetchContent yet.

Specific targets to define:

- `tlsf` — already exists as `STATIC`; confirm include path is `external_libraries/tlsf` and keep as-is
- `oscpp` — add `INTERFACE` target with `target_include_directories(oscpp INTERFACE external_libraries/oscpp/include)`
- `boost_headers` — add `INTERFACE` target with `target_include_directories(boost_headers SYSTEM INTERFACE external_libraries/boost)`
- `tinydir` — add `INTERFACE` target with `target_include_directories(tinydir INTERFACE external_libraries/tinydir)`

## Acceptance criteria

- [ ] `cmake --preset debug` defines all four targets (`tlsf`, `oscpp`, `boost_headers`, `tinydir`)
- [ ] `cmake --build --preset debug` compiles `tlsf` without errors
- [ ] No target uses a raw `include_directories()` call — all include paths flow through target properties

## Blocked by

- `01-cmake-project-scaffold-and-presets.md`
