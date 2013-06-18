#include <jni.h>
#include <errno.h>

#include <android/sensor.h>
#include <android/log.h>
#include <android_native_app_glue.h>

#include <cstring>
#include <memory>
#include <thread>
#include <unistd.h>

#if defined(__clang__)
# pragma clang diagnostic push
# pragma clang diagnostic ignored "-Wunused-private-field"
#endif
# define CATCH_CONFIG_RUNNER
# include <catch.hpp>
#if defined(__clang__)
# pragma clang diagnostic pop
#endif

extern const char* __progname;

static void run_tests()
{
    const int logLevel = ANDROID_LOG_INFO;

    // Create a default config object
    Catch::Config config;

    // Configure CATCH to send all its output to a stringstream
    std::ostringstream oss;
    config.setStreamBuf(oss.rdbuf());

    std::unique_ptr<char[]> progname(new char[strlen(__progname)+1]);
    strcpy(progname.get(), __progname);
    const size_t argc = 1;
    char* const argv[] = { progname.get() };

    __android_log_print(logLevel, progname.get(), "Running unit tests ...");

    // Forward on to CATCH's main, but using our custom config.
    // CATCH will still parse the command line and set the config
    // object up further
    int result = Catch::Main(argc, argv, config);

    __android_log_print(logLevel, progname.get(), "%s", oss.str().c_str());
    __android_log_print(logLevel, progname.get(), "Unit tests returned %d", result);
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
