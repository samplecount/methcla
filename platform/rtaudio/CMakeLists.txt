include(FetchContent)

FetchContent_Declare(
    rtaudio
    GIT_REPOSITORY https://github.com/thestk/rtaudio.git
    GIT_TAG        b9468aa6f82fdea1f3dc364f477e859b39f0bb2a
)

FetchContent_GetProperties(rtaudio)
if(NOT rtaudio_POPULATED)
    FetchContent_Populate(rtaudio)
    add_subdirectory(${rtaudio_SOURCE_DIR} ${rtaudio_BINARY_DIR})
endif()

add_library(driver_rtaudio STATIC
    Methcla/Audio/IO/RtAudioDriver
)

target_include_directories(driver_rtaudio
    PRIVATE
    .
    ../../include
    ../../external_libraries/oscpp/include
    ../../external_libraries/tlsf
    ../../src
    ${rtaudio_SOURCE_DIR}
)

target_link_libraries(driver_rtaudio rtaudio_static)
