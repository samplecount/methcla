find_package(PkgConfig)

set(plugins
    disksampler
    node_control
    patch_cable
    sampler
    sine.c
    soundfile_api_dummy
)

pkg_check_modules(soundfile_api_libsndfile sndfile REQUIRED)
if (${soundfile_api_libsndfile_FOUND})
    list(APPEND plugins soundfile_api_libsndfile)
endif ()

pkg_check_modules(soundfile_api_mpg123 libmpg123 REQUIRED)
if (${soundfile_api_mpg123_FOUND})
    list(APPEND plugins soundfile_api_mpg123)
endif ()

if (APPLE)
    find_library(CoreFoundation_LIB CoreFoundation)
    find_library(AudioToolbox_LIB AudioToolbox)
    set(soundfile_api_extaudiofile_LIBRARIES
        ${CoreFoundation_LIB}
        ${AudioToolbox_LIB}
    )
    list(APPEND plugins soundfile_api_extaudiofile)
endif ()

foreach(plugin ${plugins})
    get_filename_component(plugin_target_base ${plugin} NAME_WE)
    set(plugin_target "methcla_plugin_${plugin_target_base}")
    add_library(${plugin_target} MODULE ${plugin})
    set_target_properties(${plugin_target} PROPERTIES PREFIX "")
    target_include_directories(${plugin_target} PUBLIC
        $<BUILD_INTERFACE:${CMAKE_CURRENT_SOURCE_DIR}/../include>
        $<BUILD_INTERFACE:${CMAKE_CURRENT_SOURCE_DIR}/../external_libraries/oscpp/include>
        $<INSTALL_INTERFACE:include>
    )
    if (plugin MATCHES ".*\.c$")
        # C99 doesn't allow to not declare a parameter name
        set(c99_warnings -Wno-unused-parameter)
        target_compile_options(${plugin_target} PRIVATE
          "$<$<CXX_COMPILER_ID:Clang>:${c99_warnings}>"
          "$<$<CXX_COMPILER_ID:AppleClang>:${c99_warnings}>"
          "$<$<CXX_COMPILER_ID:GNU>:${c99_warnings}>"
        )
    endif ()
    target_link_options(${plugin_target} PUBLIC "${${plugin_target_base}_LDFLAGS}")
    target_link_libraries(${plugin_target} PUBLIC "${${plugin_target_base}_LIBRARIES}")
    target_compile_options(${plugin_target} PRIVATE "${${plugin_target_base}_CFLAGS}")
endforeach ()
