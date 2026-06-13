// Copyright (C) 2026 Methcla contributors
//
// SPDX-License-Identifier: Apache-2.0

#include "methcla_tests.hpp"
#include "test_resource.hpp"

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
