cmake_minimum_required(VERSION 3.9)
project(methcla VERSION 0.3.0)

if (CMAKE_SYSTEM_NAME STREQUAL "Linux")
    set(LINUX TRUE)
endif ()

set(CMAKE_CXX_STANDARD 11)
set(CMAKE_C_STANDARD 99)

set(warnings
    -Wall
    -Wextra
    -Wstrict-aliasing
    -Werror)

add_compile_options(
  "$<$<CXX_COMPILER_ID:Clang>:${warnings}>"
  "$<$<CXX_COMPILER_ID:AppleClang>:${warnings}>"
  "$<$<CXX_COMPILER_ID:GNU>:${warnings}>"
)

enable_testing()

add_subdirectory(platform)
add_subdirectory(plugins)
add_subdirectory(src)
add_subdirectory(tests)
