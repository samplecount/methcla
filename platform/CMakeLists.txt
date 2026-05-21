if(METHCLA_ENABLE_RTAUDIO)
    add_subdirectory(rtaudio)
else()
    add_library(driver_dummy STATIC DummyPlatform.cpp)
    target_include_directories(driver_dummy PRIVATE
        ../include
        ../src
    )
endif()

