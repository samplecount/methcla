cmake_minimum_required(VERSION 3.9)
project(methcla VERSION 0.3.0)

if (UNIX and not APPLE)
    set(LINUX TRUE)
endif ()

set(CMAKE_CXX_STANDARD 11)
set(CMAKE_C_STANDARD 99)

enable_testing()

add_subdirectory(plugins)
add_subdirectory(src)
add_subdirectory(tests)

