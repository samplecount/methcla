// Copyright 2012-2013 Samplecount S.L.
//
// SPDX-License-Identifier: Apache-2.0

#ifndef METHCLA_AUDIO_IO_RTAUDIO_DRIVER_HPP
#define METHCLA_AUDIO_IO_RTAUDIO_DRIVER_HPP

#include "Methcla/Audio/IO/Driver.hpp"
#include "Methcla/Audio/MultiChannelBuffer.hpp"

#include "RtAudio.h"

namespace Methcla { namespace Audio { namespace IO {
    class RtAudioDriver : public Driver
    {
        RtAudio                             m_audio;
        double                              m_sampleRate;
        bool                                m_isOpen;
        bool                                m_isRunning;
        std::unique_ptr<MultiChannelBuffer> m_inputBuffer;
        std::unique_ptr<MultiChannelBuffer> m_outputBuffer;

    public:
        RtAudioDriver(Options options);
        virtual ~RtAudioDriver();

        virtual double sampleRate() const override;
        virtual size_t numInputs() const override;
        virtual size_t numOutputs() const override;
        virtual size_t bufferSize() const override;

        virtual Methcla_Time currentTime() override;

        virtual void start() override;
        virtual void stop() override;

    private:
        static int processCallback(void* outputBuffer, void* inputBuffer,
                                   unsigned int numFrames, double streamTime,
                                   RtAudioStreamStatus status, void* data);
        void       process(float* outputBuffer, const float* inputBuffer,
                           unsigned int numFrames, double streamTime);
    };
}; }; }; // namespace Methcla::Audio::IO

#endif // METHCLA_AUDIO_IO_RTAUDIO_DRIVER_HPP
