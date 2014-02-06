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

#ifndef METHCLA_AUDIO_IO_DRIVER_HPP_INCLUDED
#define METHCLA_AUDIO_IO_DRIVER_HPP_INCLUDED

#include "Methcla/Audio.hpp"
#include "Methcla/Audio/MultiChannelBuffer.hpp"

#include <cstdint>
#include <cstring>

namespace Methcla { namespace Audio { namespace IO {

class Driver : public Methcla::Audio::TimeInterface
{
public:
    struct Options
    {
        double sampleRate = -1.;
        int numInputs     = -1;
        int numOutputs    = -1;
        int bufferSize    = -1;
    };

    typedef void (*ProcessCallback)(
        void* data,
        Methcla_Time currentTime,
        size_t numFrames,
        const sample_t* const* inputs,
        sample_t* const* outputs
        );

    Driver(Options options);
    virtual ~Driver();

    void setProcessCallback(ProcessCallback callback, void* data);

    virtual double sampleRate() const = 0;
    virtual size_t numInputs() const = 0;
    virtual size_t numOutputs() const = 0;
    virtual size_t bufferSize() const = 0;

    virtual Methcla_Time currentTime();

    virtual void start() = 0;
    virtual void stop() = 0;

    static Methcla_AudioSample** makeBuffers(size_t numChannels, size_t numFrames)
    {
        return MultiChannelBuffer::makeBuffers(numChannels, numFrames);
    }

    static void freeBuffers(size_t numChannels, Methcla_AudioSample** buffers)
    {
        MultiChannelBuffer::freeBuffers(numChannels, buffers);
    }

protected:
    void process(Methcla_Time currentTime, size_t numFrames, const sample_t* const* inputs, sample_t* const* outputs);

private:
    ProcessCallback m_processCallback;
    void*           m_processData;
};

} } }

#endif // METHCLA_AUDIO_IO_DRIVER_HPP_INCLUDED
