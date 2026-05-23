cmake_minimum_required(VERSION 3.24)

project(methcla VERSION 0.3.0)

set(METHCLA_SOURCE_DIR "${CMAKE_CURRENT_SOURCE_DIR}")

option(METHCLA_ENABLE_RTAUDIO "Enable RtAudio audio driver" ON)
option(METHCLA_BUILD_TESTS "Build the test suite" ${PROJECT_IS_TOP_LEVEL})

if(CMAKE_SYSTEM_NAME STREQUAL "Linux")
    set(LINUX TRUE)
endif()

set(CMAKE_CXX_STANDARD 17)
set(CMAKE_C_STANDARD 99)

set(methcla_warnings
    -Wall
    -Wextra
    -Wstrict-aliasing
    -Werror)

function(methcla_target_warnings target)
    target_compile_options(${target} PRIVATE
        "$<$<CXX_COMPILER_ID:Clang>:${methcla_warnings}>"
        "$<$<CXX_COMPILER_ID:AppleClang>:${methcla_warnings}>"
        "$<$<CXX_COMPILER_ID:GNU>:${methcla_warnings}>"
    )
endfunction()

include(GNUInstallDirs)
include(CMakePackageConfigHelpers)
include(CTest)
include(cmake/dependencies.cmake)

add_subdirectory(external_libraries)
add_subdirectory(platform)
add_subdirectory(plugins)
add_subdirectory(src)
if(METHCLA_BUILD_TESTS)
    add_subdirectory(tests)
endif()
if(PROJECT_IS_TOP_LEVEL)
    add_subdirectory(tools)
endif()

install(EXPORT methclaTargets
    NAMESPACE methcla::
    DESTINATION ${CMAKE_INSTALL_LIBDIR}/cmake/methcla)

write_basic_package_version_file(
    "${CMAKE_CURRENT_BINARY_DIR}/methclaConfigVersion.cmake"
    VERSION ${PROJECT_VERSION}
    COMPATIBILITY SameMajorVersion)

install(FILES
    cmake/methclaConfig.cmake
    "${CMAKE_CURRENT_BINARY_DIR}/methclaConfigVersion.cmake"
    DESTINATION ${CMAKE_INSTALL_LIBDIR}/cmake/methcla)
