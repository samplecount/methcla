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

#include <methcla/engine.hpp>

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
