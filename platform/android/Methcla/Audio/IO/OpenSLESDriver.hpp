// Copyright (C) 2012-2013 Samplecount S.L.
// Copyright (C) 2026 Methcla contributors
//
// SPDX-License-Identifier: Apache-2.0

#ifndef METHCLA_AUDIO_IO_OPENSLESDRIVER_HPP
#define METHCLA_AUDIO_IO_OPENSLESDRIVER_HPP

#include "Methcla/Audio.hpp"
#include "Methcla/Audio/IO/Driver.hpp"
#include "Methcla/Audio/MultiChannelBuffer.hpp"

#include "opensl_io.h"

#include <atomic>

namespace Methcla { namespace Audio { namespace IO {
    class OpenSLESDriver : public Driver
    {
    public:
        OpenSLESDriver(Options options);
        virtual ~OpenSLESDriver();

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
        static void processCallback(void* context, int sample_rate,
                                    int buffer_frames, int input_channels,
                                    const short* input_buffer,
                                    int output_channels, short* output_buffer);

    private:
        OPENSL_STREAM*        m_stream;
        double                m_sampleRate;
        size_t                m_numInputs;
        size_t                m_numOutputs;
        size_t                m_bufferSize;
        MultiChannelBuffer    m_inputBuffer;
        MultiChannelBuffer    m_outputBuffer;
        std::atomic<uint64_t> m_frameCount;
        double                m_sampleRateRecip;
    };
}; }; }; // namespace Methcla::Audio::IO

#endif // METHCLA_AUDIO_IO_OPENSLESDRIVER_HPP
