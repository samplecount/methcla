add_library(gtest 
    ../external_libraries/gtest/gtest-all.cc
)

target_include_directories(gtest PUBLIC
    ../external_libraries/gtest
)

add_executable(methcla_tests
    test_runner_console.cpp
    methcla_tests.cpp
)

target_include_directories(methcla_tests PRIVATE
    ../src
    ../external_libraries/boost
)

target_link_libraries(methcla_tests PRIVATE
    gtest
    methcla
    sine
)

add_test(methcla_tests methcla_tests)

add_executable(methcla_engine_tests
    test_runner_console.cpp
    ../external_libraries/gtest/gtest-all.cc
    methcla_engine_tests.cpp
)

target_include_directories(methcla_engine_tests PRIVATE
    ../src # For Methcla/Utility/Macros.h
    ../external_libraries/boost
    ../external_libraries/gtest
)

target_link_libraries(methcla_engine_tests PRIVATE
    gtest
    methcla
    node-control
    sine
)

add_test(methcla_engine_tests methcla_engine_tests)

