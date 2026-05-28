// Copyright 2012-2013 Samplecount S.L.
//
// SPDX-License-Identifier: Apache-2.0

#ifndef METHCLA_AUDIO_IO_REMOTEIODRIVER_HPP
#define METHCLA_AUDIO_IO_REMOTEIODRIVER_HPP

#include "Methcla/Audio/IO/Driver.hpp"
#include "Methcla/Audio/MultiChannelBuffer.hpp"

#include <atomic>
#include <cstdint>
#include <thread>

namespace Methcla { namespace Audio { namespace IO {
    class DummyDriver : public Driver
    {
    public:
        const double kDefaultSampleRate = 44100;
        const size_t kDefaultNumInputs = 2;
        const size_t kDefaultNumOutputs = 2;
        const size_t kDefaultBufferSize = 512;

        DummyDriver(Options options);
        virtual ~DummyDriver();

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

        virtual Methcla_Time currentTime() override;

    private:
        void run();

    private:
        double                m_sampleRate;
        size_t                m_numInputs;
        size_t                m_numOutputs;
        size_t                m_bufferSize;
        MultiChannelBuffer    m_inputBuffer;
        MultiChannelBuffer    m_outputBuffer;
        std::atomic<bool>     m_continue;
        std::atomic<uint64_t> m_time;
        std::thread           m_thread;
    };
}; }; }; // namespace Methcla::Audio::IO

#endif // METHCLA_AUDIO_IO_REMOTEIODRIVER_HPP
