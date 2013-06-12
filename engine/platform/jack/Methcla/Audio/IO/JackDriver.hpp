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

#ifndef METHCLA_AUDIO_IO_JACKDRIVER_HPP
#define METHCLA_AUDIO_IO_JACKDRIVER_HPP

#include "Methcla/Audio/IO/Driver.hpp"

#include <cstdint>
#include <jack/jack.h>

namespace Methcla { namespace Audio { namespace IO
{
    class JackDriver : public Driver
    {
    public:
        JackDriver();
        virtual ~JackDriver();

        virtual double sampleRate() const override { return m_sampleRate; }
        virtual size_t numInputs() const override { return m_numInputs; }
        virtual size_t numOutputs() const override { return m_numOutputs; }
        virtual size_t bufferSize() const override { return m_bufferSize; }

        virtual void start() override;
        virtual void stop() override;

    private:
        static int sampleRateCallback(jack_nframes_t nframes, void* arg);
        static int bufferSizeCallback(jack_nframes_t nframes, void* arg);
        static int processCallback(jack_nframes_t nframes, void* arg);

    private:
        double              m_sampleRate;
        size_t              m_numInputs;
        size_t              m_numOutputs;
        size_t              m_bufferSize;
        jack_client_t*      m_jackClient;
        jack_port_t**       m_jackInputPorts;
        jack_port_t**       m_jackOutputPorts;
        sample_t**          m_inputBuffers;
        sample_t**          m_outputBuffers;
    };
}; }; };

#endif // METHCLA_AUDIO_IO_JACKDRIVER_HPP
