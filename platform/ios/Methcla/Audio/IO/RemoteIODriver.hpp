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

#include <AudioUnit/AudioUnit.h>

namespace Methcla { namespace Audio { namespace IO {
    class RemoteIODriver : public Driver
    {
    public:
        RemoteIODriver(Options options, bool initializeAudioSession = true);
        virtual ~RemoteIODriver();

        virtual double sampleRate() const override { return m_sampleRate; }
        virtual size_t numInputs() const override { return m_numInputs; }
        virtual size_t numOutputs() const override { return m_numOutputs; }
        virtual size_t bufferSize() const override { return m_bufferSize; }

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
        double     m_sampleRate;
        size_t     m_numInputs;
        size_t     m_numOutputs;
        size_t     m_bufferSize;
        AudioUnit  m_rioUnit;
        sample_t** m_inputBuffers;
        sample_t** m_outputBuffers;
    };
}; }; }; // namespace Methcla::Audio::IO

#endif // METHCLA_AUDIO_IO_REMOTEIODRIVER_HPP
