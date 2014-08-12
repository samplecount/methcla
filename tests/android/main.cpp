#include <jni.h>
#include <errno.h>

#include <android/sensor.h>
#include <android/log.h>
#include <android_native_app_glue.h>

#include <cstring>
#include <sstream>
#include <string>
#include <thread>
#include <unistd.h>

#if 0
#if defined(__clang__)
# pragma clang diagnostic push
# pragma clang diagnostic ignored "-Wunused-private-field"
#endif
# define CATCH_CONFIG_RUNNER
# include <catch.hpp>
#if defined(__clang__)
# pragma clang diagnostic pop
#endif
#endif

extern const char* __progname;

static void run_tests()
{
    const std::string progname(__progname);
    const int logLevel = ANDROID_LOG_INFO;

    __android_log_print(logLevel, progname.c_str(), "Running unit tests ...");

#if 0
    // Configure CATCH to send all its output to a stringstream
    std::stringstream oss;
    Catch::Ptr<Catch::Config> config = new Catch::Config();
    config->setStreamBuf(oss.rdbuf());
    Catch::Runner runner(config);
    const int result = runner.runTests().assertions.failed;
#else
#   warning Android unit test runner needs to be ported to gtest!
    std::stringstream oss;
    oss << "FIXME: Android unit test runner needs to be ported to gtest!";
    const int result = 1;
#endif

    __android_log_print(logLevel, progname.c_str(), "%s", oss.str().c_str());
    __android_log_print(logLevel, progname.c_str(), "Unit tests returned %d", result);
}

extern "C" void android_main(struct android_app* state)
{
    // Make sure glue isn't stripped.
    app_dummy();

    run_tests();

    // Read all pending events.
    int ident;
    int events;
    struct android_poll_source* source;

    // If not animating, we will block forever waiting for events.
    // If animating, we loop until all events are read, then continue
    // to draw the next frame of animation.
    while ((ident=ALooper_pollAll(-1, NULL, &events, (void**)&source)) >= 0) {
        // Process this event.
        if (source != nullptr) {
            source->process(state, source);
        }
        // Check if we are exiting.
        if (state->destroyRequested != 0) {
            break;
        }
    }
}
