configure_file(Methcla/Version.h.in Methcla/Version.h ESCAPE_QUOTES)

set(METHCLA_PLUGIN_EXTENSION "${CMAKE_SHARED_MODULE_SUFFIX}")
configure_file(Methcla/Config.h.in Methcla/Config.h)

set(sources
    Methcla/API.cpp
    Methcla/API.hpp
    Methcla/Audio/AudioBus.cpp
    Methcla/Audio/AudioBus.hpp
    Methcla/Audio/DSP.c
    Methcla/Audio/DSP.h
    Methcla/Audio/Engine.cpp
    Methcla/Audio/Engine.hpp
    Methcla/Audio/EngineImpl.cpp
    Methcla/Audio/EngineImpl.hpp
    Methcla/Audio/Group.cpp
    Methcla/Audio/Group.hpp
    Methcla/Audio/IO/Driver.cpp
    Methcla/Audio/IO/Driver.hpp
    Methcla/Audio/IO/DummyDriver.cpp
    Methcla/Audio/IO/DummyDriver.hpp
    Methcla/Audio/MultiChannelBuffer.hpp
    Methcla/Audio/Node.cpp
    Methcla/Audio/Node.hpp
    Methcla/Audio/Resource.cpp
    Methcla/Audio/Resource.hpp
    Methcla/Audio/Synth.cpp
    Methcla/Audio/Synth.hpp
    Methcla/Audio/SynthDef.cpp
    Methcla/Audio/SynthDef.hpp
    Methcla/Audio.hpp
    Methcla/Exception.hpp
    Methcla/Faust.hpp
    Methcla/Memory/Manager.cpp
    Methcla/Memory/Manager.hpp
    Methcla/Memory.cpp
    Methcla/Memory.hpp
    Methcla/Platform.hpp
    Methcla/Plugin/Loader.cpp
    Methcla/Plugin/Loader.hpp
    Methcla/Plugin/Manager.cpp
    Methcla/Plugin/Manager.hpp
    Methcla/Utility/Hash.hpp
    Methcla/Utility/Macros.h
    Methcla/Utility/MessageQueue.hpp
    Methcla/Utility/MessageQueueInterface.hpp
    Methcla/Utility/Semaphore.cpp
    Methcla/Utility/Semaphore.hpp
    Methcla/Utility/WorkerInterface.hpp
)

add_library(methcla ${sources})

target_include_directories(methcla PUBLIC
    $<BUILD_INTERFACE:${CMAKE_CURRENT_SOURCE_DIR}/../include>
    $<BUILD_INTERFACE:${CMAKE_CURRENT_SOURCE_DIR}/../external_libraries/oscpp/include>
    $<INSTALL_INTERFACE:include>
)

target_include_directories(methcla
    PRIVATE
    ${CMAKE_CURRENT_SOURCE_DIR}
    ${CMAKE_CURRENT_BINARY_DIR}
    ../external_libraries
    ../external_libraries/tinydir
)

target_include_directories(methcla
    SYSTEM PRIVATE
    ../external_libraries/boost
)

target_link_libraries(methcla
    PRIVATE
    tlsf
    driver_rtaudio)

if (LINUX)
    target_link_libraries(methcla
        PUBLIC
        dl
        pthread
    )
endif ()
