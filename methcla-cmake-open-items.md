# Methcla — CMake Migration Open Items
*Updated: DS-CMAKE-01*

---

## Design Sessions Required

### DS-CMAKE-02: Shared Library Build

Decide whether methcla should support a shared library (`.dylib`/`.so`) build variant in addition to the static library. Requires resolving symbol visibility strategy (`-fvisibility=hidden` baseline with explicit `METHCLA_API` export macro) and whether the `methcla::methcla` target should be an alias for whichever variant is selected at configure time or two separate targets.

Questions to resolve:
- Is a shared library needed for any current downstream consumer?
- Should `BUILD_SHARED_LIBS` be respected, or should methcla always build static?
- What is the public symbol set that needs to be exported?

### DS-CMAKE-03: Boost Dependency Elimination

Evaluate replacing `boost::lockfree` and `boost::heap` with alternatives to remove the vendored Boost headers entirely.

Questions to resolve:
- Can `boost::lockfree::spsc_queue` be replaced with a standalone lock-free queue (e.g. `rigtorp/SPSCQueue`, which is a single header)?
- Can `boost::heap` usage be replaced with `std::priority_queue` or a small standalone library?
- Is the Boost header footprint worth reducing, or is vendoring acceptable long-term?

Dependency: resolves issue #27 (replace `boost::noncopyable`) as a prerequisite.

---

## Deferred Features (Backlog)

### BL-CM-01: iOS and Android Cross-Compilation

CMake supports Android NDK toolchain files natively and iOS via `CMAKE_SYSTEM_NAME=iOS`. Deferred until the desktop build is stable. Will require separate presets and likely a separate `CMakePresets.json` or a `CMakePresets.json` inheritance chain.

### BL-CM-02: Package Manager Integration (vcpkg / Conan)

vcpkg manifest mode or a Conan 2 `conanfile.py` could replace FetchContent for libsndfile and Catch2, giving better caching and version pinning. Low priority while FetchContent is sufficient.

### BL-CM-03: Shared Library Variant

See DS-CMAKE-02. Deferred until the static build is proven.

### BL-CM-04: CI/CD Pipeline

GitHub Actions (or equivalent) configuration using `cmake --preset` directly. Straightforward once presets are finalized. Should run both debug and release builds and execute CTest.

---

## Open Questions

### OQ-CM-01: GIT_TAG Pinning Strategy for FetchContent

FetchContent dependencies should be pinned to specific tags or commit SHAs for reproducibility. Need to decide: pin to release tags (e.g. `libsndfile-1.2.2`) or to commit SHAs? Tags are readable; SHAs are immutable. Convention should be established before the first commit.

### OQ-CM-02: Vendored Boost Header Extraction Tool

The existing `external_libraries/boost` directory contains a manually curated subset of Boost headers. If the subset needs to be updated, `bcp` (Boost's own extraction tool) is the right approach. Should the extraction command be documented in the repo (e.g. a script or README note) so the process is reproducible?

### OQ-CM-03: Static vs. Object Library for tlsf

tlsf is two C files. It can be compiled as a `STATIC` library or as an `OBJECT` library (no archive, sources linked directly into methcla). `OBJECT` avoids a redundant archive step but is slightly less conventional. Decide before implementation.

### OQ-CM-04: Minimum Deployment Target (macOS)

`CMAKE_OSX_DEPLOYMENT_TARGET` should be set explicitly. Current Shake build behavior on this is not documented. What is the minimum macOS version methcla targets?

---

## Action Items

| ID | Item | Dependency |
|---|---|---|
| AI-CM-01 | Create `CMakeLists.txt` scaffold: project, version, C++11 standard, install dirs | — |
| AI-CM-02 | Add FetchContent blocks for oscpp, tlsf, libsndfile, Catch2 with pinned tags (resolve OQ-CM-01 first) | AI-CM-01 |
| AI-CM-03 | Define `methcla` target with correct `PUBLIC`/`PRIVATE` include and link structure | AI-CM-02 |
| AI-CM-04 | Add install/export block: `install(TARGETS)`, `install(EXPORT)`, `methclaConfig.cmake` | AI-CM-03 |
| AI-CM-05 | Add `CMakePresets.json` with debug and release configure presets | AI-CM-01 |
| AI-CM-06 | Migrate compiler flags from Shake `.cfg` files to `target_compile_options` | AI-CM-03 |
| AI-CM-07 | Add test target gated on `METHCLA_BUILD_TESTS`, wire up `catch_discover_tests` and CTest | AI-CM-03 |
| AI-CM-08 | Verify `add_subdirectory` consumption works with a minimal test consumer project | AI-CM-04 |
| AI-CM-09 | Verify `find_package` consumption works after `cmake --install` | AI-CM-04 |
| AI-CM-10 | Document `FETCHCONTENT_SOURCE_DIR_*` offline override pattern in README | AI-CM-02 |
| AI-CM-11 | Resolve OQ-CM-04 (macOS deployment target) and set in presets | AI-CM-05 |
