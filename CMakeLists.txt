cmake_minimum_required(VERSION 3.21)

if(NOT DEFINED CMAKE_OSX_DEPLOYMENT_TARGET)
    set(CMAKE_OSX_DEPLOYMENT_TARGET "26" CACHE STRING "Minimum macOS version")
endif()

project(methcla VERSION 0.3.0)

option(METHCLA_ENABLE_RTAUDIO "Enable RtAudio audio driver" ON)
option(METHCLA_BUILD_TESTS "Build the test suite" OFF)

if(CMAKE_SYSTEM_NAME STREQUAL "Linux")
    set(LINUX TRUE)
endif()

set(CMAKE_CXX_STANDARD 11)
set(CMAKE_C_STANDARD 99)

set(methcla_warnings
    -Wall
    -Wextra
    -Wstrict-aliasing
    -Werror)

add_compile_options(
    "$<$<CXX_COMPILER_ID:Clang>:${methcla_warnings}>"
    "$<$<CXX_COMPILER_ID:AppleClang>:${methcla_warnings}>"
    "$<$<CXX_COMPILER_ID:GNU>:${methcla_warnings}>"
)

enable_testing()

add_subdirectory(external_libraries)
add_subdirectory(platform)
add_subdirectory(plugins)
add_subdirectory(src)
if(METHCLA_BUILD_TESTS)
    add_subdirectory(tests)
endif()
