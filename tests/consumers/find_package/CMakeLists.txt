cmake_minimum_required(VERSION 3.24)
project(methcla_consumer_find_package C CXX)

find_package(methcla REQUIRED)

add_executable(consumer main.c)
target_link_libraries(consumer PRIVATE methcla::methcla)
