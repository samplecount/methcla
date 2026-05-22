cmake_minimum_required(VERSION 3.24)
project(methcla_consumer_add_subdirectory C CXX)

add_subdirectory(../../.. methcla)

add_executable(consumer main.c)
target_link_libraries(consumer PRIVATE methcla::methcla)
