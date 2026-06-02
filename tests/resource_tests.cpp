// Copyright (C) 2026 Methcla contributors
//
// SPDX-License-Identifier: Apache-2.0

#include "methcla_tests.hpp"
#include "test_resource.hpp"

#include <methcla/engine.h>
#include <methcla/engine.hpp>

#include "gtest/gtest.h"

using namespace Methcla::Tests;

// Wait for /resource/ready on the given id, asserting it arrives within
// timeout.
static void awaitReady(Methcla::Engine& engine, Methcla::ResourceId id,
                       const char* uri)
{
    AsyncLatch latch;
    engine.addNotificationHandler([&, id](const OSCPP::Server::Message& msg) {
        if (msg == "/resource/ready" && msg.args().int32() == id.id())
        {
            latch.signal();
            return true;
        }
        return false;
    });
    Methcla::Request req(engine);
    req.openBundle();
    req.resourceNew(id, uri);
    req.closeBundle();
    req.send();
    ASSERT_TRUE(latch.wait()) << "Timed out waiting for /resource/ready";
}

TEST(ResourceTests, UnknownUriDoesNotNotifyReady)
{
    auto engine = std::make_unique<Methcla::Engine>();
    engine->start();

    AsyncLatch          latch;
    Methcla::ResourceId id = engine->resourceIdAllocator().alloc();

    engine->addNotificationHandler([&, id](const OSCPP::Server::Message& msg) {
        if (msg == "/resource/ready" && msg.args().int32() == id.id())
        {
            latch.signal();
            return true;
        }
        return false;
    });

    Methcla::Request req(*engine);
    req.openBundle();
    req.resourceNew(id, "methcla://plugins/unknown");
    req.closeBundle();
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

    Methcla::ResourceId id = engine->resourceIdAllocator().alloc();
    awaitReady(*engine, id, METHCLA_TEST_RESOURCE_URI);

    engine->resourceIdAllocator().free(id);
    engine->stop();
}

TEST(ResourceTests, FreeLiveResourceDestroysIt)
{
    Methcla::EngineOptions opts;
    opts.addLibrary(methcla_plugins_test_resource);
    auto engine = std::make_unique<Methcla::Engine>(opts);
    engine->start();

    Methcla::ResourceId id = engine->resourceIdAllocator().alloc();
    awaitReady(*engine, id, METHCLA_TEST_RESOURCE_URI);

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
        req.resourceFree(id);
        req.closeBundle();
        req.send();
    }
    EXPECT_TRUE(destroyed.wait());
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

    Methcla::ResourceId id = engine->resourceIdAllocator().alloc();

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

    // Both commands in one bundle so they are processed in the same RT
    // callback.
    {
        Methcla::Request req(*engine);
        req.openBundle();
        req.resourceNew(id, METHCLA_TEST_RESOURCE_URI);
        req.resourceFree(id);
        req.closeBundle();
        req.send();
    }

    EXPECT_TRUE(destroyed.wait(std::chrono::milliseconds(1000)));
    // /resource/ready must NOT have been emitted.
    EXPECT_FALSE(ready.wait(std::chrono::milliseconds(0)));
    engine->resourceIdAllocator().free(id);
    engine->stop();
}
