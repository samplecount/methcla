include(FetchContent)

# oscpp — header-only OSC library, part of methcla's public API.
# Use Populate (not MakeAvailable) to skip oscpp's CMakeLists.txt, which adds
# tests that depend on absent submodules and would pollute the CTest registry.
# The scoped CMP0169=OLD silences the FetchContent_Populate deprecation warning
# on CMake >= 3.28 while keeping the policy at its default everywhere else.
FetchContent_Declare(oscpp
    GIT_REPOSITORY https://github.com/kaoskorobase/oscpp.git
    GIT_TAG 805365c3b7b7a5c819866040ab434f011dfcfbf9
    FIND_PACKAGE_ARGS CONFIG
)
FetchContent_GetProperties(oscpp)
if(NOT oscpp_POPULATED)
    cmake_policy(PUSH)
    if(POLICY CMP0169)
        cmake_policy(SET CMP0169 OLD)
    endif()
    FetchContent_Populate(oscpp)
    cmake_policy(POP)
    add_library(oscpp INTERFACE)
    target_include_directories(oscpp SYSTEM INTERFACE
        $<BUILD_INTERFACE:${oscpp_SOURCE_DIR}/include>
        $<INSTALL_INTERFACE:include>
    )
endif()

# googletest — test-only dependency
FetchContent_Declare(googletest
    GIT_REPOSITORY https://github.com/google/googletest.git
    GIT_TAG v1.14.0
    FIND_PACKAGE_ARGS NAMES GTest
)
set(INSTALL_GTEST OFF CACHE BOOL "" FORCE)
FetchContent_MakeAvailable(googletest)
