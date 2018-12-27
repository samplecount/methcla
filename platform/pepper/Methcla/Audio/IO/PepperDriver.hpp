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

#ifndef METHCLA_AUDIO_IO_PEPPER_DRIVER_HPP
#define METHCLA_AUDIO_IO_PEPPER_DRIVER_HPP

#include "Methcla/Audio/IO/Driver.hpp"
#include "Methcla/Audio/MultiChannelBuffer.hpp"

#include "ppapi/cpp/audio.h"

#include <atomic>
#include <memory>

namespace Methcla { namespace Audio { namespace IO {
    class PepperDriver : public Driver
    {
    public:
        PepperDriver(Options options, const pp::InstanceHandle& instance);
        virtual ~PepperDriver();

        virtual double sampleRate() const override;
        virtual size_t numInputs() const override;
        virtual size_t numOutputs() const override;
        virtual size_t bufferSize() const override;

        virtual Methcla_Time currentTime() override;

        virtual void start() override;
        virtual void stop() override;

    private:
        static void processCallback(void* samples, uint32_t buffer_size,
                                    void* data);

    private:
        pp::Audio                           m_audio;
        std::unique_ptr<MultiChannelBuffer> m_outputBuffer;
        std::atomic<uint64_t>               m_frameCount;
        double                              m_sampleRateRecip;
    };
}; }; }; // namespace Methcla::Audio::IO

#endif // METHCLA_AUDIO_IO_PEPPER_DRIVER_HPP
