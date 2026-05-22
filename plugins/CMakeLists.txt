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
elseif (LINUX)
    find_package(PkgConfig REQUIRED)
    pkg_check_modules(sndfile sndfile)
    if (sndfile_FOUND)
        add_methcla_plugin(soundfile_api_libsndfile soundfile_api_libsndfile.cpp)
        target_compile_options(methcla_plugin_soundfile_api_libsndfile PRIVATE ${sndfile_CFLAGS})
        target_link_libraries(methcla_plugin_soundfile_api_libsndfile PRIVATE ${sndfile_LIBRARIES})
    else ()
        message(STATUS "libsndfile not found — skipping soundfile_api_libsndfile")
    endif ()
endif ()

if (EXISTS "${CMAKE_CURRENT_SOURCE_DIR}/soundfile_api_mpg123.cpp")
    find_package(PkgConfig)
    if (PkgConfig_FOUND)
        pkg_check_modules(mpg123 libmpg123)
        if (mpg123_FOUND)
            add_methcla_plugin(soundfile_api_mpg123 soundfile_api_mpg123.cpp)
            target_compile_options(methcla_plugin_soundfile_api_mpg123 PRIVATE ${mpg123_CFLAGS})
            target_link_libraries(methcla_plugin_soundfile_api_mpg123 PRIVATE ${mpg123_LIBRARIES})
        else ()
            message(STATUS "libmpg123 not found — skipping soundfile_api_mpg123")
        endif ()
    endif ()
endif ()
