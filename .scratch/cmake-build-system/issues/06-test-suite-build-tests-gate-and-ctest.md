Status: ready-for-agent

# 06 — Test suite: `METHCLA_BUILD_TESTS` gate and CTest wiring

## What to build

Gate the entire `tests/` build on `METHCLA_BUILD_TESTS=ON` (default `OFF`) so library consumers don't build gtest and the test executables when embedding methcla. When enabled, the existing gtest-based tests build and register with CTest unchanged.

Specific changes:
- In `tests/CMakeLists.txt`, wrap all content in `if(METHCLA_BUILD_TESTS)`
- Confirm `configure_file(config.cpp.in config.cpp)` still generates the correct plugin directory paths pointing at `${CMAKE_BINARY_DIR}/plugins` (from issue #05)
- Confirm both `add_test()` registrations are present: `methcla_tests` and `methcla_engine_tests`
- Suppress gtest warnings with `target_compile_options(gtest PRIVATE -Wno-missing-field-initializers -Wno-unused-const-variable)` as currently done

## Acceptance criteria

- [ ] `cmake --preset debug` (without `-DMETHCLA_BUILD_TESTS=ON`) does not build gtest or any test executable
- [ ] `cmake --preset debug -DMETHCLA_BUILD_TESTS=ON && cmake --build --preset debug` produces both test executables
- [ ] `ctest --preset debug` (or `ctest --test-dir build/debug`) runs both tests and reports results
- [ ] Both tests pass

## Blocked by

- `04-methcla-target-structure-and-alias.md`
- `05-plugin-module-targets-and-soundfile-api-selection.md`
