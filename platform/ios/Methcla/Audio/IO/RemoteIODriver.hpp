// Copyright (C) 2012-2013 Samplecount S.L.
// Copyright (C) 2026 Methcla contributors
//
// SPDX-License-Identifier: Apache-2.0

#pragma once

#include "Methcla/Audio/IO/Driver.hpp"
#include "Methcla/Audio/MultiChannelBuffer.hpp"

#include <memory>

#include <AudioUnit/AudioUnit.h>

namespace Methcla { namespace Audio { namespace IO {
    class RemoteIODriver : public Driver
    {
    public:
        RemoteIODriver(Options options, bool initializeAudioSession = true);
        virtual ~RemoteIODriver();

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

        virtual Methcla_Time currentTime() override;

        virtual void start() override;
        virtual void stop() override;

        AudioUnit audioUnit();

    private:
        static void     InterruptionCallback(void*  inClientData,
                                             UInt32 inInterruption);
        static OSStatus InputCallback(void*                       inRefCon,
                                      AudioUnitRenderActionFlags* ioActionFlags,
                                      const AudioTimeStamp*       inTimeStamp,
                                      UInt32 inBusNumber, UInt32 inNumberFrames,
                                      AudioBufferList* ioData);
        static OSStatus
        RenderCallback(void*                       inRefCon,
                       AudioUnitRenderActionFlags* ioActionFlags,
                       const AudioTimeStamp* inTimeStamp, UInt32 inBusNumber,
                       UInt32 inNumberFrames, AudioBufferList* ioData);

    private:
        double                              m_sampleRate;
        size_t                              m_numInputs;
        size_t                              m_numOutputs;
        size_t                              m_bufferSize;
        AudioUnit                           m_rioUnit;
        std::unique_ptr<MultiChannelBuffer> m_inputBuffer;
        std::unique_ptr<MultiChannelBuffer> m_outputBuffer;
    };
}; }; }; // namespace Methcla::Audio::IO
