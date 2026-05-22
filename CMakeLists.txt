cmake_minimum_required(VERSION 3.24)

project(methcla VERSION 0.3.0)

option(METHCLA_ENABLE_RTAUDIO "Enable RtAudio audio driver" ON)

if(CMAKE_SYSTEM_NAME STREQUAL "Linux")
    set(LINUX TRUE)
endif()

set(CMAKE_CXX_STANDARD 14)
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

include(CTest)
include(cmake/dependencies.cmake)

add_subdirectory(external_libraries)
add_subdirectory(platform)
add_subdirectory(plugins)
add_subdirectory(src)
if(BUILD_TESTING)
    add_subdirectory(tests)
endif()
