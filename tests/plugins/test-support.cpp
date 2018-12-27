// Copyright 2016 Samplecount S.L.
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

#include "plugins/test-support.h"

#include <methcla/plugin.hpp>

#include <cmath>

using namespace Methcla::Plugin;

namespace {

    // TestStats

    enum TestStatsType
    {
        kTestStat_MaxAbsAmp
    };

    constexpr size_t kMaxNumStats = 8;

    class TestStatsOptions
    {
        void addStat(TestStatsType type) { m_stats[m_numStats++] = type; }

    public:
        TestStatsOptions(OSCPP::Server::ArgStream args)
        {
            m_skipFrames = args.int32();
            m_collectFrames = args.int32();

            m_numStats = 0;
            while (!args.atEnd() && m_numStats < kMaxNumStats)
            {
                const std::string stat(args.string());
                if (stat == "MaxAbsAmp")
                {
                    addStat(kTestStat_MaxAbsAmp);
                }
            }
        }

        TestStatsOptions(const TestStatsOptions& other)
        {
            std::memcpy(this, &other, sizeof(other));
        }

        size_t        m_skipFrames;
        size_t        m_collectFrames;
        size_t        m_numStats;
        TestStatsType m_stats[kMaxNumStats];
    };

    class TestStatsPorts
    {
    public:
        typedef size_t Port;

        static constexpr size_t numPorts() { return 2; }

        static Methcla_PortDescriptor descriptor(Port port)
        {
            if (port < numPorts())
            {
                return Methcla::Plugin::PortDescriptor::audioInput();
            }
            else
            {
                throw std::runtime_error("Invalid port index");
            }
        }
    };

    class MaxAbsAmp
    {
        float m_result;

    public:
        MaxAbsAmp()
        : m_result(0.f)
        {}

        void process(const float* const* inputs, size_t numInputs,
                     size_t startFrame, size_t numFrames)
        {
            for (size_t c = 0; c < numInputs; c++)
            {
                for (size_t k = startFrame; k < startFrame + numFrames; k++)
                {
                    m_result = std::max(m_result, std::fabs(inputs[c][k]));
                }
            }
        }

        TestStatsType type() const { return kTestStat_MaxAbsAmp; }

        float result() const { return m_result; }
    };

    class TestStats
    {
        int32_t       m_skipFrames;
        int32_t       m_collectFrames;
        TestStatsType m_stats[1];
        bool          m_done;
        const float*  m_inputs[TestStatsPorts::numPorts()];

        MaxAbsAmp m_maxAbsAmp;

        void report(const World<TestStats>& world, TestStatsType type,
                    float value)
        {
            switch (type)
            {
                case kTestStat_MaxAbsAmp:
                    world.log(kMethcla_LogInfo)
                        << METHCLA_TEST_STATS_OUTPUT_PREFIX
                        << "MaxAbsAmp=" << value;
                    break;
            }
        }

    public:
        TestStats(const World<TestStats>& world, const Methcla_SynthDef*,
                  const TestStatsOptions& options)
        : m_skipFrames(options.m_skipFrames)
        , m_collectFrames(options.m_collectFrames)
        , m_done(false)
        {
            m_stats[0] = options.m_stats[0];
            std::fill(m_inputs, m_inputs + TestStatsPorts::numPorts(), nullptr);
        }

        void connect(TestStatsPorts::Port port, void* data)
        {
            if (port < TestStatsPorts::numPorts())
            {
                m_inputs[port] = static_cast<const float*>(data);
            }
            else
            {
                throw std::runtime_error("Invalid port index");
            }
        }

        void process(const World<TestStats>& world, size_t numFrames)
        {
            if (!m_done)
            {
                size_t startFrame = 0;
                size_t activeNumFrames = 0;

                if (m_skipFrames > 0)
                {
                    if (m_skipFrames > numFrames)
                    {
                        m_skipFrames -= numFrames;
                    }
                    else
                    {
                        startFrame = m_skipFrames;
                        activeNumFrames = numFrames - m_skipFrames;
                        m_skipFrames = 0;
                    }
                }
                else if (m_collectFrames > 0)
                {
                    if (m_collectFrames > numFrames)
                    {
                        m_collectFrames -= numFrames;
                        activeNumFrames = numFrames;
                    }
                    else
                    {
                        activeNumFrames = m_collectFrames;
                        m_collectFrames = 0;
                    }
                }

                if (activeNumFrames >= m_collectFrames)
                {
                    m_collectFrames = 0;
                }

                m_maxAbsAmp.process(m_inputs, TestStatsPorts::numPorts(),
                                    startFrame, activeNumFrames);

                if (m_collectFrames == 0)
                {
                    report(world, m_maxAbsAmp.type(), m_maxAbsAmp.result());
                    m_done = true;
                    world.synthDone(this);
                }
            }
        }
    };

    StaticSynthDef<TestStats, TestStatsOptions, TestStatsPorts> kTestStatsDef;

    // Library
    const Methcla_Library library = {NULL, NULL};

}; // namespace

METHCLA_EXPORT const Methcla_Library*
                     methcla_plugins_test_support(const Methcla_Host* host,
                                                  const char* /* bundlePath */)
{
    kTestStatsDef(host, METHCLA_PLUGINS_TEST_STATS_URI);
    return &library;
}
