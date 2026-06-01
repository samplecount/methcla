if(METHCLA_BUILD_TESTS)

    # =============================================================================
    # Plugin directories

    set(PLUGIN_DIRECTORIES "\"${CMAKE_BINARY_DIR}/plugins\""
                           "\"${CMAKE_CURRENT_BINARY_DIR}/plugins\"")
    string(REPLACE ";" "," PLUGIN_DIRECTORIES "${PLUGIN_DIRECTORIES}")

    configure_file(config.cpp.in config.cpp)

    target_compile_options(gtest PRIVATE -Wno-missing-field-initializers
                                         -Wno-unused-const-variable)

    # =============================================================================
    # methcla_tests

    add_executable(
        methcla_tests
        methcla_tests.cpp
        semaphore_tests.cpp
        worker_tests.cpp
        memory_tests.cpp
        engine_tests.cpp
        resource_tests.cpp
        ${CMAKE_CURRENT_BINARY_DIR}/config.cpp)

    target_include_directories(methcla_tests PRIVATE . ../src)
    target_include_directories(methcla_tests SYSTEM
                               PRIVATE ../external_libraries/boost)

    target_link_libraries(methcla_tests PRIVATE GTest::gtest methcla)
    methcla_target_warnings(methcla_tests)

    add_test(methcla_tests methcla_tests)

endif() # METHCLA_BUILD_TESTS
