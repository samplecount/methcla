configure_file(Methcla/Version.h.in Methcla/Version.h ESCAPE_QUOTES)

set(METHCLA_PLUGIN_EXTENSION "${CMAKE_SHARED_MODULE_SUFFIX}")
configure_file(Methcla/Config.h.in Methcla/Config.h)

set(sources
    Methcla/API.cpp
    Methcla/API.hpp
    Methcla/Audio/AudioBus.cpp
    Methcla/Audio/AudioBus.hpp
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
    Methcla/Plugin/Loader.hpp
    Methcla/Plugin/LoaderPosix.cpp
    Methcla/Plugin/Manager.cpp
    Methcla/Plugin/Manager.hpp
    Methcla/Utility/Hash.hpp
    Methcla/Utility/Macros.h
    Methcla/Utility/MessageQueue.hpp
    Methcla/Utility/MessageQueueInterface.hpp
    Methcla/Utility/Semaphore.cpp
    Methcla/Utility/Semaphore.hpp
    Methcla/Utility/WorkerInterface.hpp)

add_library(methcla ${sources})

target_sources(
    methcla
    PUBLIC
        FILE_SET
        HEADERS
        BASE_DIRS
        ${METHCLA_SOURCE_DIR}/include
        FILES
        ${METHCLA_SOURCE_DIR}/include/methcla/common.h
        ${METHCLA_SOURCE_DIR}/include/methcla/detail.hpp
        ${METHCLA_SOURCE_DIR}/include/methcla/detail/result.hpp
        ${METHCLA_SOURCE_DIR}/include/methcla/engine.h
        ${METHCLA_SOURCE_DIR}/include/methcla/engine.hpp
        ${METHCLA_SOURCE_DIR}/include/methcla/file.h
        ${METHCLA_SOURCE_DIR}/include/methcla/file.hpp
        ${METHCLA_SOURCE_DIR}/include/methcla/log.h
        ${METHCLA_SOURCE_DIR}/include/methcla/log.hpp
        ${METHCLA_SOURCE_DIR}/include/methcla/platform/ios.h
        ${METHCLA_SOURCE_DIR}/include/methcla/platform/rtaudio.hpp
        ${METHCLA_SOURCE_DIR}/include/methcla/plugin.h
        ${METHCLA_SOURCE_DIR}/include/methcla/plugin.hpp
        ${METHCLA_SOURCE_DIR}/include/methcla/plugins/disksampler.h
        ${METHCLA_SOURCE_DIR}/include/methcla/plugins/node_control.h
        ${METHCLA_SOURCE_DIR}/include/methcla/plugins/patch_cable.h
        ${METHCLA_SOURCE_DIR}/include/methcla/plugins/sampler.h
        ${METHCLA_SOURCE_DIR}/include/methcla/plugins/sine.h
        ${METHCLA_SOURCE_DIR}/include/methcla/plugins/soundfile_api_dummy.h
        ${METHCLA_SOURCE_DIR}/include/methcla/plugins/soundfile_api_extaudiofile.h
        ${METHCLA_SOURCE_DIR}/include/methcla/plugins/soundfile_api_libsndfile.h
        ${METHCLA_SOURCE_DIR}/include/methcla/types.h)

target_include_directories(
    methcla PUBLIC $<BUILD_INTERFACE:${METHCLA_SOURCE_DIR}/include>
                   $<INSTALL_INTERFACE:${CMAKE_INSTALL_INCLUDEDIR}>)

install(
    TARGETS methcla
    EXPORT methclaTargets
    ARCHIVE DESTINATION ${CMAKE_INSTALL_LIBDIR}
            FILE_SET HEADERS
            DESTINATION ${CMAKE_INSTALL_INCLUDEDIR})

target_include_directories(
    methcla PRIVATE ${CMAKE_CURRENT_SOURCE_DIR} ${CMAKE_CURRENT_BINARY_DIR}
                    ../external_libraries)

if(METHCLA_ENABLE_RTAUDIO)
    set(methcla_driver driver_rtaudio)
else()
    set(methcla_driver driver_dummy)
endif()

target_link_libraries(
    methcla
    PUBLIC oscpp::oscpp
    PRIVATE $<BUILD_INTERFACE:boost_headers> $<BUILD_INTERFACE:tlsf>
            $<BUILD_INTERFACE:${methcla_driver}>)

# GCC false positives triggered by third-party headers (boost::lockfree)
target_compile_options(
    methcla
    PRIVATE
        "$<$<CXX_COMPILER_ID:GNU>:-Wno-array-bounds;-Wno-stringop-overflow;-Wno-restrict>"
)

if(LINUX)
    target_link_libraries(methcla PUBLIC atomic dl pthread)
endif()

methcla_target_warnings(methcla)

add_library(methcla::methcla ALIAS methcla)
