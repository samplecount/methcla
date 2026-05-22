# Dependency management modernisation (FetchContent, Boost 1.91, C++17)

## Fetched dependencies via FetchContent

oscpp and googletest are declared in `cmake/dependencies.cmake` with `FetchContent_Declare` / `FetchContent_MakeAvailable` rather than git submodules or vendored source. This keeps the repository small, makes version bumps a one-line change, and allows `FIND_PACKAGE_ARGS` to reuse a system-installed copy when available. The file is included from the top-level `CMakeLists.txt` before any `add_subdirectory` so the targets are visible everywhere.

## Vendored dependencies as CMake targets

tlsf and tinydir remain vendored under `external_libraries/` because they are single-file or single-header libraries with no upstream CMake packaging worth depending on. Each is wrapped in a proper CMake target (`OBJECT` for tlsf, `INTERFACE` for tinydir) rather than a raw include-path injection, so consumers link by target name and transitive include paths are handled automatically.

tlsf uses `OBJECT` so its translation unit is merged directly into `libmethcla` and does not appear as a separate link-time dependency for users of the library.

## Boost vendored with methcla_boost namespace

Only the required subset (lockfree, heap, container_hash, smart_ptr) is vendored. `tools/copy-boost.sh <version>` downloads the official tarball and runs `bcp --namespace methcla_boost` to rename the `boost::` namespace, preventing symbol collisions when a host application links its own Boost. To update Boost, re-run the script with the new version and commit the result.

## C++ standard set to 17

The codebase targets C++17. The primary driver was Boost 1.91 lockfree requiring `std::conditional_t` (C++14 minimum), but we went directly to C++17 to allow use of structured bindings, `if constexpr`, `std::optional`, and other widely-supported features without further standard bumps in the near term. C++17 has been supported by all targeted compilers (GCC, Clang, AppleClang) for several years.
