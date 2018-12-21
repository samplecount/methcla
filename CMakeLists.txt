cmake_minimum_required(VERSION 3.9)
project(methcla VERSION 0.3.0)

set(CMAKE_CXX_STANDARD 11)

enable_testing()

add_subdirectory(plugins)
add_subdirectory(src)
add_subdirectory(tests)

