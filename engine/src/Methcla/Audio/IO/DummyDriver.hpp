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

#ifndef METHCLA_AUDIO_IO_REMOTEIODRIVER_HPP
#define METHCLA_AUDIO_IO_REMOTEIODRIVER_HPP

#include "Methcla/Audio/IO/Driver.hpp"

#include <atomic>
#include <thread>

namespace Methcla { namespace Audio { namespace IO
{
    class DummyDriver : public Driver
    {
    public:
        const double kDefaultSampleRate = 44100;
        const size_t kDefaultNumInputs = 2;
        const size_t kDefaultNumOutputs = 2;
        const size_t kDefaultBufferSize = 512;

        DummyDriver(Options options);
        virtual ~DummyDriver();

        virtual double sampleRate() const override { return m_sampleRate; }
        virtual size_t numInputs() const override { return m_numInputs; }
        virtual size_t numOutputs() const override { return m_numOutputs; }
        virtual size_t bufferSize() const override { return m_bufferSize; }

        virtual void start() override;
        virtual void stop() override;

    private:
        void run();

    private:
        double              m_sampleRate;
        size_t              m_numInputs;
        size_t              m_numOutputs;
        size_t              m_bufferSize;
        sample_t**          m_inputBuffers;
        sample_t**          m_outputBuffers;
        std::atomic<bool>   m_continue;
        std::atomic<double> m_time;
        std::thread         m_thread;
    };
}; }; };

#endif // METHCLA_AUDIO_IO_REMOTEIODRIVER_HPP
