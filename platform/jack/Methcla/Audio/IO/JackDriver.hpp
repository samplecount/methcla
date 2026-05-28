// Copyright (C) 2012-2013 Samplecount S.L.
// Copyright (C) 2026 Methcla contributors
//
// SPDX-License-Identifier: Apache-2.0

#pragma once

#include "Methcla/Audio/IO/Driver.hpp"

#include <cstdint>

#include <jack/jack.h>

namespace Methcla { namespace Audio { namespace IO {
    class JackDriver : public Driver
    {
    public:
        JackDriver(Options options);
        virtual ~JackDriver();

        virtual double sampleRate() const override
        {
            return m_sampleRate;
        }
        virtual size_t numInputs() const override
        {
            return m_numInputs;
        }
        virtual size_t numOutputs() const override
        {
            return m_numOutputs;
        }
        virtual size_t bufferSize() const override
        {
            return m_bufferSize;
        }

        virtual void start() override;
        virtual void stop() override;

    private:
        static int sampleRateCallback(jack_nframes_t nframes, void* arg);
        static int bufferSizeCallback(jack_nframes_t nframes, void* arg);
        static int processCallback(jack_nframes_t nframes, void* arg);

    private:
        double         m_sampleRate;
        size_t         m_numInputs;
        size_t         m_numOutputs;
        size_t         m_bufferSize;
        jack_client_t* m_jackClient;
        jack_port_t**  m_jackInputPorts;
        jack_port_t**  m_jackOutputPorts;
        sample_t**     m_inputBuffers;
        sample_t**     m_outputBuffers;
    };
}; }; }; // namespace Methcla::Audio::IO
