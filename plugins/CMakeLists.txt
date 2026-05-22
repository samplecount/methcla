function(add_methcla_plugin name source)
    set(target "methcla_plugin_${name}")
    add_library(${target} MODULE ${source})
    set_target_properties(${target} PROPERTIES
        PREFIX ""
        LIBRARY_OUTPUT_DIRECTORY "${CMAKE_BINARY_DIR}/plugins"
    )
    target_include_directories(${target} PRIVATE
        $<BUILD_INTERFACE:${CMAKE_CURRENT_SOURCE_DIR}/../include>
    )
    target_link_libraries(${target} PRIVATE oscpp::oscpp)
    methcla_target_warnings(${target})
    if (source MATCHES ".*\\.c$")
        target_compile_options(${target} PRIVATE
            "$<$<CXX_COMPILER_ID:Clang>:-Wno-unused-parameter>"
            "$<$<CXX_COMPILER_ID:AppleClang>:-Wno-unused-parameter>"
            "$<$<CXX_COMPILER_ID:GNU>:-Wno-unused-parameter>"
        )
    endif()
endfunction()

foreach(source IN ITEMS
    disksampler.cpp
    node_control.cpp
    patch_cable.cpp
    sampler.cpp
    sine.c
    soundfile_api_dummy.cpp
)
    get_filename_component(name ${source} NAME_WE)
    add_methcla_plugin(${name} ${source})
endforeach()

if (APPLE)
    find_library(CoreFoundation_LIB CoreFoundation REQUIRED)
    find_library(AudioToolbox_LIB AudioToolbox REQUIRED)
    add_methcla_plugin(soundfile_api_extaudiofile soundfile_api_extaudiofile.cpp)
    target_link_libraries(methcla_plugin_soundfile_api_extaudiofile PRIVATE
        ${CoreFoundation_LIB}
        ${AudioToolbox_LIB}
    )
endif ()

find_package(PkgConfig)
if (PkgConfig_FOUND)
    pkg_check_modules(sndfile IMPORTED_TARGET sndfile)
    if (sndfile_FOUND)
        add_methcla_plugin(soundfile_api_libsndfile soundfile_api_libsndfile.cpp)
        target_link_libraries(methcla_plugin_soundfile_api_libsndfile PRIVATE PkgConfig::sndfile)
    else ()
        message(STATUS "libsndfile not found — skipping soundfile_api_libsndfile")
    endif ()
endif ()
