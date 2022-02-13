find_package(PkgConfig)

pkg_check_modules(rtaudio rtaudio REQUIRED IMPORTED_TARGET)

add_library(driver_rtaudio STATIC
    Methcla/Audio/IO/RtAudioDriver
)

target_include_directories(driver_rtaudio
    PRIVATE
    .
    ../../include
    ../../external_libraries/boost
    ../../external_libraries/oscpp/include
    ../../src
)

target_link_libraries(driver_rtaudio
    PUBLIC
    PkgConfig::rtaudio
)
