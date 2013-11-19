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

#if defined(__clang__)
# pragma clang diagnostic push
# pragma clang diagnostic ignored "-Wunused-private-field"
#endif

#include <catch.hpp>

#if defined(__clang__)
# pragma clang diagnostic pop
#endif

#include <methcla/engine.h>
#include <methcla/engine.hpp>
#include <methcla/plugins/sine.h>

#include <chrono>

TEST_CASE("methcla/engine/creation", "Test engine creation and tear down.")
{
    Methcla_Engine* engine = nullptr;
    Methcla_Error result;
    result = methcla_engine_new(nullptr, nullptr, nullptr, &engine);
    REQUIRE(result == kMethcla_NoError);
    REQUIRE(engine);
    result = methcla_engine_start(engine);
    REQUIRE(result == kMethcla_NoError);
    result = methcla_engine_stop(engine);
    REQUIRE(result == kMethcla_NoError);
    methcla_engine_free(engine);
}

TEST_CASE("methcla/engine/node/free_crash", "Freeing an invalid node id shouldn't crash the engine.")
{
    auto engine = std::unique_ptr<Methcla::Engine>(
        new Methcla::Engine()
    );

    engine->start();
    engine->free(-1);
    std::this_thread::sleep_for(std::chrono::duration<double>(0.25));
    engine->stop();

    REQUIRE(true);
}

TEST_CASE("Shouldn't be able to add message to closed request bundle", "[api]")
{
    auto engine = std::unique_ptr<Methcla::Engine>(
        new Methcla::Engine()
    );
    Methcla::Request request(*engine);
    request.openBundle(0.);
    request.closeBundle();
    REQUIRE_THROWS( request.group(engine->root()) );
}

TEST_CASE("Node tree should only contain root node after startup", "[engine]")
{
    auto engine = std::unique_ptr<Methcla::Engine>(
        new Methcla::Engine(
            { Methcla::Option::pluginLibrary(methcla_plugins_sine) }
        )
    );
    // engine->setLogFlags(kMethcla_EngineLogRequests);
    engine->start();
    // Methcla::Request request(*engine);
    // request.openBundle(0.);
    // request.closeBundle();
    // REQUIRE_THROWS( request.group(engine->root()) );
    const Methcla::NodeTreeStatistics stats = engine->getNodeTreeStatistics();
    REQUIRE( stats.numGroups == 1 );
    REQUIRE( stats.numSynths == 0 );
}
