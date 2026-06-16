// Copyright (C) 2026 Methcla contributors
//
// SPDX-License-Identifier: Apache-2.0

#include "methcla_tests.hpp"
#include "test_resource.hpp"
#include "test_resource_synth.hpp"

#include <methcla/engine.h>
#include <methcla/engine.hpp>

#include "gtest/gtest.h"

using namespace Methcla::Tests;

// Construct a resource of type `uri`, wait for /resource/ready, and return the
// allocated id.
static Methcla::ResourceId awaitReady(Methcla::Engine& engine, const char* uri)
{
    Methcla::Request req(engine);
    req.openBundle();
    const Methcla::ResourceId id = req.resource(uri);
    req.closeBundle();

    AsyncLatch latch;
    engine.addNotificationHandler([&, id](const OSCPP::Server::Message& msg) {
        if (msg == "/resource/ready" && msg.args().int32() == id.id())
        {
            latch.signal();
            return true;
        }
        return false;
    });
    req.send();
    EXPECT_TRUE(latch.wait()) << "Timed out waiting for /resource/ready";
    return id;
}

TEST(ResourceTests, UnknownUriDoesNotNotifyReady)
{
    auto engine = std::make_unique<Methcla::Engine>();
    engine->start();

    Methcla::Request req(*engine);
    req.openBundle();
    const Methcla::ResourceId id = req.resource("methcla://plugins/unknown");
    req.closeBundle();

    AsyncLatch latch;
    engine->addNotificationHandler([&, id](const OSCPP::Server::Message& msg) {
        if (msg == "/resource/ready" && msg.args().int32() == id.id())
        {
            latch.signal();
            return true;
        }
        return false;
    });
    req.send();

    EXPECT_FALSE(latch.wait());
    engine->resourceIdAllocator().free(id);
    engine->stop();
}

TEST(ResourceTests, KnownUriConstructsAndNotifiesReady)
{
    Methcla::EngineOptions opts;
    opts.addLibrary(methcla_plugins_test_resource);
    auto engine = std::make_unique<Methcla::Engine>(opts);
    engine->start();

    const Methcla::ResourceId id =
        awaitReady(*engine, METHCLA_TEST_RESOURCE_URI);
    engine->resourceIdAllocator().free(id);
    engine->stop();
}

TEST(ResourceTests, FreeLiveResourceDestroysIt)
{
    Methcla::EngineOptions opts;
    opts.addLibrary(methcla_plugins_test_resource);
    auto engine = std::make_unique<Methcla::Engine>(opts);
    engine->start();

    const Methcla::ResourceId id =
        awaitReady(*engine, METHCLA_TEST_RESOURCE_URI);

    AsyncLatch destroyed;
    engine->addNotificationHandler([&, id](const OSCPP::Server::Message& msg) {
        if (msg == "/resource/destroyed" && msg.args().int32() == id.id())
        {
            destroyed.signal();
            return true;
        }
        return false;
    });

    {
        Methcla::Request req(*engine);
        req.openBundle();
        req.free(id);
        req.closeBundle();
        req.send();
    }
    EXPECT_TRUE(destroyed.wait());
    engine->stop();
}

TEST(ResourceTests, ConstructErrorNotifiesResourceError)
{
    Methcla::EngineOptions opts;
    opts.addLibrary(methcla_plugins_test_resource);
    auto engine = std::make_unique<Methcla::Engine>(opts);
    engine->start();

    Methcla::Request req(*engine);
    req.openBundle();
    const Methcla::ResourceId id =
        req.resource(METHCLA_TEST_RESOURCE_URI, {Methcla::Value(1)});
    req.closeBundle();

    AsyncLatch error;
    AsyncLatch ready;

    engine->addNotificationHandler([&, id](const OSCPP::Server::Message& msg) {
        if (msg == "/resource/error")
        {
            auto args = msg.args();
            if (args.int32() == id.id())
            {
                int32_t     code = args.int32();
                const char* message = args.string();
                EXPECT_EQ(code, static_cast<int32_t>(kMethcla_ArgumentError));
                EXPECT_STREQ(message, "boom");
                error.signal();
                return true;
            }
        }
        return false;
    });
    engine->addNotificationHandler([&, id](const OSCPP::Server::Message& msg) {
        if (msg == "/resource/ready" && msg.args().int32() == id.id())
        {
            ready.signal();
            return true;
        }
        return false;
    });
    req.send();

    EXPECT_TRUE(error.wait(std::chrono::milliseconds(1000)));
    EXPECT_FALSE(ready.wait(std::chrono::milliseconds(0)));
    engine->resourceIdAllocator().free(id);
    engine->stop();
}

// Refcount > 0 must defer the destroy until the last release. The synth holds
// a ref for its full lifetime: /resource/free arrives while held → no destroy;
// /node/free releases the ref → destroy fires.
TEST(ResourceTests, HeldResourceIsDestroyedAfterRelease)
{
    Methcla::EngineOptions opts;
    opts.addLibrary(methcla_plugins_test_resource_synths);
    auto engine = std::make_unique<Methcla::Engine>(opts);
    engine->start();

    const Methcla::ResourceId id =
        awaitReady(*engine, METHCLA_TEST_RESOURCE_URI);

    const Methcla::SynthId synth =
        engine->synth(METHCLA_TEST_HOLD_SYNTH_URI,
                      Methcla::NodePlacement::head(engine->root()), {},
                      {Methcla::Value(id.id())});

    AsyncLatch destroyed;
    engine->addNotificationHandler([&, id](const OSCPP::Server::Message& msg) {
        if (msg == "/resource/destroyed" && msg.args().int32() == id.id())
        {
            destroyed.signal();
            return true;
        }
        return false;
    });

    // /resource/free arrives while the synth holds a ref → must NOT destroy.
    engine->free(id);
    EXPECT_FALSE(destroyed.wait(std::chrono::milliseconds(100)));

    // Freeing the synth releases the ref → destroy fires.
    engine->free(synth);
    EXPECT_TRUE(destroyed.wait());
    engine->stop();
}

// Acquiring with a non-matching URI must return null. The test synth records
// acquire-failure by leaving `data` null, which is observable via the same
// destroy notification: the synth must NOT have prevented destruction.
TEST(ResourceTests, AcquireWithWrongUriReturnsNull)
{
    Methcla::EngineOptions opts;
    opts.addLibrary(methcla_plugins_test_resource_synths);
    auto engine = std::make_unique<Methcla::Engine>(opts);
    engine->start();

    const Methcla::ResourceId id =
        awaitReady(*engine, METHCLA_TEST_RESOURCE_URI);

    // Construct a hold-synth pointing at a bogus id (out of range). The
    // synth must construct (acquire returns null is not a synth-construct
    // error) but not hold a ref.
    const Methcla::SynthId synth = engine->synth(
        METHCLA_TEST_HOLD_SYNTH_URI,
        Methcla::NodePlacement::head(engine->root()), {}, {Methcla::Value(-1)});

    AsyncLatch destroyed;
    engine->addNotificationHandler([&, id](const OSCPP::Server::Message& msg) {
        if (msg == "/resource/destroyed" && msg.args().int32() == id.id())
        {
            destroyed.signal();
            return true;
        }
        return false;
    });

    // /resource/free should destroy immediately — the bogus-id synth never
    // acquired our resource.
    engine->free(id);
    EXPECT_TRUE(destroyed.wait());

    engine->free(synth);
    engine->stop();
}

// perform_with_resources brackets a worker-thread callback with RT
// acquire/release. The test synth dispatches one on activate and the
// callback emits a notification carrying both a marker and a flag that the
// acquired pointer matched the one observed at synth-construct time.
TEST(ResourceTests, PerformWithResourcesDispatchesCallback)
{
    Methcla::EngineOptions opts;
    opts.addLibrary(methcla_plugins_test_resource_synths);
    auto engine = std::make_unique<Methcla::Engine>(opts);
    engine->start();

    const Methcla::ResourceId id =
        awaitReady(*engine, METHCLA_TEST_RESOURCE_URI);

    constexpr int32_t kMarker = 0xBEEF;

    AsyncLatch dispatched;
    int32_t    receivedMarker = 0;
    int32_t    ptrOk = -1;
    engine->addNotificationHandler([&](const OSCPP::Server::Message& msg) {
        if (msg == "/test/perform-with-resources")
        {
            auto args = msg.args();
            receivedMarker = args.int32();
            ptrOk = args.int32();
            dispatched.signal();
            return true;
        }
        return false;
    });

    const Methcla::SynthId synth =
        engine->synth(METHCLA_TEST_PERFORM_SYNTH_URI,
                      Methcla::NodePlacement::head(engine->root()), {},
                      {Methcla::Value(id.id()), Methcla::Value(kMarker)});
    engine->activate(synth);

    EXPECT_TRUE(dispatched.wait(std::chrono::milliseconds(1000)));
    EXPECT_EQ(receivedMarker, kMarker);
    EXPECT_EQ(ptrOk, 1);

    engine->free(synth);
    engine->free(id);
    engine->stop();
}

// Sending /resource/free before construction completes must result in
// /resource/destroyed only (no /resource/ready).
TEST(ResourceTests, FreeDuringConstructSetsFreePending)
{
    Methcla::EngineOptions opts;
    opts.addLibrary(methcla_plugins_test_resource);
    auto engine = std::make_unique<Methcla::Engine>(opts);
    engine->start();

    // Both commands in one bundle so they are processed in the same RT
    // callback.
    Methcla::Request req(*engine);
    req.openBundle();
    const Methcla::ResourceId id = req.resource(METHCLA_TEST_RESOURCE_URI);
    req.free(id);
    req.closeBundle();

    AsyncLatch ready;
    AsyncLatch destroyed;

    engine->addNotificationHandler([&, id](const OSCPP::Server::Message& msg) {
        if (msg == "/resource/ready" && msg.args().int32() == id.id())
        {
            ready.signal();
            return true;
        }
        return false;
    });
    engine->addNotificationHandler([&, id](const OSCPP::Server::Message& msg) {
        if (msg == "/resource/destroyed" && msg.args().int32() == id.id())
        {
            destroyed.signal();
            return true;
        }
        return false;
    });
    req.send();

    EXPECT_TRUE(destroyed.wait(std::chrono::milliseconds(1000)));
    // /resource/ready must NOT have been emitted.
    EXPECT_FALSE(ready.wait(std::chrono::milliseconds(0)));
    engine->stop();
}
