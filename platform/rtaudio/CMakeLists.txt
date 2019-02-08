find_path(rtaudio_INCLUDES RtAudio.h)
find_library(rtaudio_LIBS rtaudio_static)

add_library(driver_rtaudio STATIC
    Methcla/Audio/IO/RtAudioDriver
)

target_include_directories(driver_rtaudio
    PRIVATE
    .
    ../../include
    ../../external_libraries/boost
    ../../external_libraries/oscpp/include
    ../../external_libraries/tlsf
    ../../src
)

target_include_directories(driver_rtaudio PRIVATE ${rtaudio_INCLUDES})
target_link_libraries(driver_rtaudio ${rtaudio_LIBS})
