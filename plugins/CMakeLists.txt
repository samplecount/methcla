set(plugins
    disksampler.cpp
    node-control.cpp
    patch-cable.cpp
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
    get_filename_component(plugin_target ${plugin} NAME_WE)
    add_library(${plugin_target} ${plugin})
    target_include_directories(${plugin_target} PUBLIC
        $<BUILD_INTERFACE:${CMAKE_CURRENT_SOURCE_DIR}/../include>
        $<BUILD_INTERFACE:${CMAKE_CURRENT_SOURCE_DIR}/../external_libraries/oscpp/include>
        $<INSTALL_INTERFACE:include>
    )
endforeach ()

