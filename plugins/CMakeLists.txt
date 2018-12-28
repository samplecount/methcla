set(plugins
    disksampler.cpp
    node_control.cpp
    patch_cable.cpp
    sampler.cpp
    sine.c
    soundfile_api_dummy.cpp
    soundfile_api_libsndfile.cpp
    soundfile_api_mpg123.cpp
)

if (APPLE)
    set(plugins
        ${plugins}
        soundfile_api_extaudiofile.cpp
    )
endif ()

foreach(plugin ${plugins})
    get_filename_component(plugin_target_base ${plugin} NAME_WE)
    set(plugin_target "methcla_plugin_${plugin_target_base}")
    add_library(${plugin_target} ${plugin})
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
endforeach ()
