// Copyright 2012-2016 Samplecount S.L.
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
#include "plugins/test-support.h"

#include <methcla/engine.h>
#include <methcla/engine.hpp>
#include <methcla/plugins/disksampler.h>
#include <methcla/plugins/node-control.h>
#include <methcla/plugins/sine.h>
#include <methcla/plugins/soundfile_api_libsndfile.h>

#include "gtest/gtest.h"

#include <sstream>

using namespace Methcla::Tests;

// Handle output from test-stats plugin.
class TestStatsOutputHandler
{
    static std::string::const_iterator isPrefix(const std::string& s1, const std::string& s2)
    {
        std::string::const_iterator p1 = s1.begin();
        std::string::const_iterator p2 = s2.begin();
        while (p1 != s1.end() && p2 != s2.end()) {
            if (*p1 == *p2) {
                p1++; p2++;
            } else {
                return s2.end();
            }
        }
        return p2;
    }

    std::function<void(const std::string& stat, float value)> m_handler;

public:
    TestStatsOutputHandler(const std::function<void(const std::string& stat, float value)>& handler)
        : m_handler(handler)
    {}
    
    void operator()(Methcla_LogLevel level, const char* msg) const
    {
        const std::string str(msg);
        std::string::const_iterator p = isPrefix(METHCLA_TEST_STATS_OUTPUT_PREFIX, str);
        if (p != str.end()) {
            // Parse statistics type and value and pass to handler.
            std::string stats(p, str.end());
            std::istringstream is(stats);
            std::string stat;
            std::getline(is, stat, '=');
            float value;
            is >> value;
            m_handler(stat, value);
        } else {
            TEST_COUT << "METHLA: " << str << "\n";
        }
    }
};

TEST(Methcla_Engine, issue_113_disksampler)
{
    TEST_DESCRIPTION("https://github.com/samplecount/methcla/issues/113");

    const std::string soundFilePath(inputFile("issue_113_U2WrongExit_adpcm_ima_wav.wav"));

    float maxAbsAmp = 1.f;

    auto engine = std::unique_ptr<Methcla::Engine>(
        new Methcla::Engine(
            Methcla::EngineOptions()
                .addLibrary(methcla_plugins_disksampler)
                .addLibrary(methcla_soundfile_api_libsndfile)
                .addLibrary(methcla_plugins_test_support)
                // Register test-stats plugin output handler.
                .setLogHandler(TestStatsOutputHandler([&maxAbsAmp](const std::string& stat, float value) {
                    maxAbsAmp = value;
                    TEST_COUT << stat << "=" << value << "\n";
                }))
        )
    );
    
    engine->start();

    Methcla::SynthId disksampler;
    Methcla::SynthId stats;

    std::tuple<Methcla::AudioBusId,Methcla::AudioBusId> bus;
    std::get<0>(bus) = engine->audioBusId().alloc();
    std::get<1>(bus) = engine->audioBusId().alloc();

    {
        Methcla::Request request(*engine);
        request.openBundle();
            disksampler = request.synth(
                METHCLA_PLUGINS_DISKSAMPLER_URI,
                Methcla::NodePlacement::head(engine->root()),
                { 1.f
                , 1.f
                },
                { Methcla::Value(soundFilePath)
                , Methcla::Value(false) }
            );

            stats = request.synth(
                METHCLA_PLUGINS_TEST_STATS_URI,
                Methcla::NodePlacement::tail(engine->root()),
                { },
                { Methcla::Value(294930)
                , Methcla::Value(50000)
                , Methcla::Value("MaxAbsAmp") }
            );

            request.mapInput(stats, 0, std::get<0>(bus));
            request.mapInput(stats, 1, std::get<1>(bus));
            request.mapOutput(disksampler, 0, std::get<0>(bus));
            request.mapOutput(disksampler, 1, std::get<1>(bus));

            // The time offset was determined experimentally, the bug didn't show up with every offset reliably.
            // It seems to show up especially when offset/(bufferSize/sampleRate) is close to an integer.
            request.openBundle(engine->currentTime() + 0.127777);
                request.activate(disksampler);
                request.activate(stats);
            request.closeBundle();
        request.closeBundle();

        request.send();
    }

    sleepFor(8);

    ASSERT_EQ(0.f, maxAbsAmp);
}
