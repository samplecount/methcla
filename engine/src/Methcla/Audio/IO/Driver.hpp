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

#include <cstdint>
#include <cstring>

namespace Methcla { namespace Audio { namespace IO {

class Driver : public Methcla::Audio::TimeInterface
{
public:
    struct Options
    {
        double sampleRate = -1.;
        int numInputs = -1;
        int numOutputs = -1;
        int bufferSize = -1;
        int blockSize = -1;
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

    size_t blockSize() const;

    virtual Methcla_Time currentTime() const override;

    virtual void start() = 0;
    virtual void stop() = 0;

    static sample_t** makeBuffers(size_t numChannels, size_t numFrames);
    static void freeBuffers(size_t numChannels, sample_t** buffers);

protected:
    void process(Methcla_Time currentTime, size_t numFrames, const sample_t* const* inputs, sample_t* const* outputs);

private:
    ProcessCallback m_processCallback;
    void*           m_processData;
    size_t          m_blockSize;
};

class MultiChannelBuffer
{
public:
    MultiChannelBuffer(size_t numChannels, size_t numFrames)
        : m_numChannels(numChannels)
        , m_numFrames(numFrames)
        , m_data(Driver::makeBuffers(m_numChannels, m_numFrames))
    { }
    ~MultiChannelBuffer()
    {
        Driver::freeBuffers(m_numChannels, m_data);
    }

    size_t numChannels() const
    {
        return m_numChannels;
    }

    size_t numFrames() const
    {
        return m_numFrames;
    }

    Methcla_AudioSample* const* data()
    {
        return m_data;
    }

    const Methcla_AudioSample* const* data() const
    {
        return m_data;
    }

    void zero()
    {
        for (size_t i=0; i < numChannels(); i++)
        {
            std::memset(data()[i], 0, numFrames() * sizeof(Methcla_AudioSample));
        }
    }

private:
    size_t                m_numChannels;
    size_t                m_numFrames;
    Methcla_AudioSample** m_data;
};

//* Instantiate the default driver for the current platform.
Driver* defaultPlatformDriver(Driver::Options options);

} } }

#endif // METHCLA_AUDIO_IO_DRIVER_HPP_INCLUDED
