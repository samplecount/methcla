set(plugins
    disksampler.cpp
    node-control.cpp
    patch-cable.cpp
    sampler.cpp
    sine.c
    soundfile_api_dummy.cpp
    soundfile_api_extaudiofile.cpp
    soundfile_api_libsndfile.cpp
    soundfile_api_mpg123.cpp
)

foreach(plugin ${plugins})
    get_filename_component(plugin_target ${plugin} NAME_WE)
    add_library(${plugin_target} ${plugin})
    target_include_directories(${plugin_target} PUBLIC
        $<BUILD_INTERFACE:${CMAKE_CURRENT_SOURCE_DIR}/../include>
        $<INSTALL_INTERFACE:include>
    )
endforeach ()

