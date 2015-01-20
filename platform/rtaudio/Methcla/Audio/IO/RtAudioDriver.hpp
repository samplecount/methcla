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

#ifndef METHCLA_AUDIO_IO_RTAUDIO_DRIVER_HPP
#define METHCLA_AUDIO_IO_RTAUDIO_DRIVER_HPP

#include "Methcla/Audio/IO/Driver.hpp"
#include "Methcla/Audio/MultiChannelBuffer.hpp"
#include "RtAudio.h"

namespace Methcla { namespace Audio { namespace IO
{
    class RtAudioDriver : public Driver
    {
        RtAudio m_audio;
        double m_sampleRate;
        bool m_isOpen;
        bool m_isRunning;
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
        static int processCallback(void* outputBuffer, void* inputBuffer, unsigned int numFrames,
                                   double streamTime, RtAudioStreamStatus status, void* data);
        void process(float* outputBuffer, const float* inputBuffer, unsigned int numFrames, double streamTime);
    };
}; }; };

#endif // METHCLA_AUDIO_IO_RTAUDIO_DRIVER_HPP
