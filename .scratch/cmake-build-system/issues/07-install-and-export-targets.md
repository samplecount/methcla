Status: ready-for-agent

# 07 — Install and export targets (`methcla::methcla` via `find_package`)

## What to build

Add install and export rules so a consumer can use `find_package(methcla REQUIRED)` after `cmake --install` and link against `methcla::methcla` without embedding the source tree.

Specific changes:
- `include(GNUInstallDirs)` in root `CMakeLists.txt`
- In `src/CMakeLists.txt`, declare the public headers as a `FILE_SET` (CMake 3.21) and add the install rule:
  ```cmake
  install(TARGETS methcla EXPORT methclaTargets
      ARCHIVE DESTINATION ${CMAKE_INSTALL_LIBDIR}
      FILE_SET HEADERS DESTINATION ${CMAKE_INSTALL_INCLUDEDIR})
  ```
- Export the targets:
  ```cmake
  install(EXPORT methclaTargets
      NAMESPACE methcla::
      DESTINATION ${CMAKE_INSTALL_LIBDIR}/cmake/methcla)
  ```
- Write `methclaConfig.cmake` (in `cmake/`) containing only `include("${CMAKE_CURRENT_LIST_DIR}/methclaTargets.cmake")` and install it alongside the export
- Add `methclaConfigVersion.cmake` via `write_basic_package_version_file`
- Install Plugin MODULE libraries to `${CMAKE_INSTALL_LIBDIR}/methcla/plugins/`

## Acceptance criteria

- [ ] `cmake --install build/debug --prefix /tmp/methcla-install` completes without errors
- [ ] The install tree contains `include/methcla/*.h`, `lib/libmethcla.a`, `lib/cmake/methcla/methclaConfig.cmake`, and Plugin `.so`/`.dylib` files under `lib/methcla/plugins/`
- [ ] `find_package(methcla REQUIRED)` in a fresh CMake project pointing at the install prefix succeeds
- [ ] The imported `methcla::methcla` target carries the correct include path automatically

## Blocked by

- `04-methcla-target-structure-and-alias.md`
