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
#include "Methcla/Exception.hpp"
#include <boost/cstdint.hpp>

namespace Methcla { namespace Audio { namespace IO
{
struct Exception : virtual Methcla::Exception { };

class Driver
{
public:
    typedef void (*ProcessCallback)(void* data, size_t numFrames, const sample_t* const* inputs, sample_t* const* outputs);

    Driver();
    virtual ~Driver();

    void setProcessCallback(ProcessCallback callback, void* data);

    virtual double sampleRate() const = 0;
    virtual size_t numInputs() const = 0;
    virtual size_t numOutputs() const = 0;
    virtual size_t bufferSize() const = 0;

    virtual void start() = 0;
    virtual void stop() = 0;

    static sample_t** makeBuffers(size_t numChannels, size_t numFrames);
    static void freeBuffers(size_t numChannels, sample_t** buffers);

protected:
    void process(size_t numFrames, const sample_t* const* inputs, sample_t* const* outputs);

private:
    ProcessCallback m_processCallback;
    void*           m_processData;
};

//* Instantiate the default driver for the current platform.
Driver* defaultPlatformDriver();

} } }

#endif // METHCLA_AUDIO_IO_DRIVER_HPP_INCLUDED
