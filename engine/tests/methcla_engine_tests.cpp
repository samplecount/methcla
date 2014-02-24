// Copyright 2012-2013 Samplecount S.L.
//
// Licensed under the Apache License, Version 2.0 (the "License");
// you may not use this file except in compliance with the License.
// You may obtain a copy of the License at
//
//     http://www.apache.org/licenses/LICENSE-2.0
//
// Unless required by applicable law or agreed to in writing, software
// distributed under the License is distributed on an "AS IS" BASIS,
// WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
// See the License for the specific language governing permissions and
// limitations under the License.

#include "methcla_tests.hpp"

#include <methcla/engine.h>
#include <methcla/engine.hpp>
#include <methcla/plugins/node-control.h>
#include <methcla/plugins/sine.h>

#include "gtest/gtest.h"

using namespace Methcla::Tests;

TEST(Methcla_Engine, Creation_and_destruction)
{
    std::unique_ptr<Methcla::Engine> engine;
    ASSERT_NO_THROW(
        engine = std::unique_ptr<Methcla::Engine>(new Methcla::Engine())
    );
    ASSERT_NO_THROW(engine->start());
    ASSERT_NO_THROW(engine->stop());
}

TEST(Methcla_Engine, Freeing_invalid_node_id_should_not_crash)
{
    auto engine = std::unique_ptr<Methcla::Engine>(
        new Methcla::Engine()
    );

    engine->start();
    engine->free(-1);
    sleepFor(0.25);
}

TEST(Methcla_Request, Should_not_be_able_to_add_message_to_closed_request_bundle)
{
    auto engine = std::unique_ptr<Methcla::Engine>(
        new Methcla::Engine()
    );
    Methcla::Request request(*engine);
    request.openBundle(0.);
    request.closeBundle();
    ASSERT_ANY_THROW( request.group(engine->root()) );
}

TEST(Methcla_Engine, Node_tree_should_contain_only_root_node_after_startup)
{
    auto engine = std::unique_ptr<Methcla::Engine>(
        new Methcla::Engine(Methcla::EngineOptions().addLibrary(methcla_plugins_sine))
    );
    // engine->setLogFlags(kMethcla_EngineLogRequests);
    engine->start();
    // Methcla::Request request(*engine);
    // request.openBundle(0.);
    // request.closeBundle();
    // REQUIRE_THROWS( request.group(engine->root()) );
    const Methcla::NodeTreeStatistics stats = engine->getNodeTreeStatistics();
    EXPECT_EQ( stats.numGroups, 1ul );
    EXPECT_EQ( stats.numSynths, 0ul );
}

TEST(Methcla_Engine, kMethcla_NodeDoneFlags_should_free_the_specified_nodes)
{
    auto engine = std::unique_ptr<Methcla::Engine>(
        new Methcla::Engine(
            Methcla::EngineOptions()
                .addLibrary(methcla_plugins_sine)
                .addLibrary(methcla_plugins_node_control)
        )
    );

    engine->start();

    // kNodeDoneFreeSelf
    {
        Methcla::Request request(*engine);
        request.openBundle();
        request.freeAll(engine->root());
        Methcla::SynthId synth = request.synth(METHCLA_PLUGINS_DONE_AFTER_URI, engine->root(), {}, { Methcla::Value(1.45e-3f) });
        request.whenDone(synth, Methcla::kNodeDoneFreeSelf);
        request.activate(synth);
        request.closeBundle();
        request.send();
    }

    EXPECT_EQ( engine->getNodeTreeStatistics().numSynths, 1ul );
    sleepFor(0.1);
    ASSERT_EQ( engine->getNodeTreeStatistics().numSynths, 0ul );

    // kNodeDoneFreePreceeding
    {
        Methcla::Request request(*engine);
        request.openBundle();
        request.freeAll(engine->root());
        request.synth(METHCLA_PLUGINS_SINE_URI, engine->root(), { 440.f, 1.f });
        Methcla::SynthId synth = request.synth(METHCLA_PLUGINS_DONE_AFTER_URI, engine->root(), {}, { Methcla::Value(0.1f) });
        request.whenDone(synth, Methcla::kNodeDoneFreePreceeding);
        request.activate(synth);
        request.closeBundle();
        request.send();
    }

    EXPECT_EQ( engine->getNodeTreeStatistics().numSynths, 2ul );
    sleepFor(0.15);
    ASSERT_EQ( engine->getNodeTreeStatistics().numSynths, 1ul );

    // kNodeDoneFreePreceeding|kNodeDoneFreeFollowing
    {
        Methcla::Request request(*engine);
        request.openBundle();
        request.freeAll(engine->root());
        request.synth(METHCLA_PLUGINS_SINE_URI, engine->root(), { 440.f, 1.f });
        Methcla::SynthId synth = request.synth(METHCLA_PLUGINS_DONE_AFTER_URI, engine->root(), {}, { Methcla::Value(0.1f) });
        request.whenDone(synth, Methcla::kNodeDoneFreePreceeding|Methcla::kNodeDoneFreeFollowing);
        request.activate(synth);
        request.synth(METHCLA_PLUGINS_SINE_URI, engine->root(), { 440.f, 1.f });
        request.closeBundle();
        request.send();
    }

    EXPECT_EQ( engine->getNodeTreeStatistics().numSynths, 3ul );
    sleepFor(0.15);
    ASSERT_EQ( engine->getNodeTreeStatistics().numSynths, 1ul );

    // kNodeDoneFreePreceeding|kNodeDoneFreeSelf|kNodeDoneFreeFollowing
    {
        Methcla::Request request(*engine);
        request.openBundle();
        request.freeAll(engine->root());
        request.synth(METHCLA_PLUGINS_SINE_URI, engine->root(), { 440.f, 1.f });
        Methcla::SynthId synth = request.synth(METHCLA_PLUGINS_DONE_AFTER_URI, engine->root(), {}, { Methcla::Value(0.033f) });
        request.whenDone(synth, Methcla::kNodeDoneFreePreceeding|Methcla::kNodeDoneFreeSelf|Methcla::kNodeDoneFreeFollowing);
        request.activate(synth);
        request.synth(METHCLA_PLUGINS_SINE_URI, engine->root(), { 440.f, 1.f });
        request.closeBundle();
        request.send();
    }

    EXPECT_EQ( engine->getNodeTreeStatistics().numSynths, 3ul );
    sleepFor(0.15);
    ASSERT_EQ( engine->getNodeTreeStatistics().numSynths, 0ul );
}

TEST(Methcla_Engine, Node_ended_notification)
{
    auto engine = std::unique_ptr<Methcla::Engine>(
        new Methcla::Engine(
            Methcla::EngineOptions()
                .addLibrary(methcla_plugins_sine)
                .addLibrary(methcla_plugins_node_control)
        )
    );

    engine->start();

    // kNodeDoneFreeSelf
    {
        Methcla::Request request(*engine);
        request.openBundle();
        request.freeAll(engine->root());
        Methcla::SynthId synth = request.synth(METHCLA_PLUGINS_DONE_AFTER_URI, engine->root(), {}, { Methcla::Value(1.45e-3f) });
        request.whenDone(synth, Methcla::kNodeDoneFreeSelf);
        request.activate(synth);
        request.closeBundle();
        // NOTE: Add notification handler before sending request in order to avoid race condition.
        engine->addNotificationHandler(engine->freeNodeIdHandler(synth));
        request.send();
    }

    EXPECT_EQ( engine->getNodeTreeStatistics().numSynths, 1ul );
    sleepFor(0.1);
    ASSERT_EQ( engine->getNodeTreeStatistics().numSynths, 0ul );
    ASSERT_EQ( engine->nodeIdAllocator().getStatistics().allocated(), 0ul );
}
