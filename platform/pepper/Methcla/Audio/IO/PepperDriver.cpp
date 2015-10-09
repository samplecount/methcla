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

#include "Methcla/Audio.hpp"
#include "Methcla/Audio/IO/PepperDriver.hpp"
#include "Methcla/API.hpp"
#include "Methcla/Exception.hpp"
#include "Methcla/Memory.hpp"
#include "Methcla/Platform.hpp"

#include <methcla/common.h>
#include <methcla/platform/pepper.hpp>

#include <cassert>
#include <cstdint>
#include <limits>
#include <stdexcept>
#include <string>

using namespace Methcla;
using namespace Methcla::Audio::IO;

PepperDriver::PepperDriver(Options options, const pp::InstanceHandle& instance)
    : Driver(options)
    , m_frameCount(0)
{
    PP_AudioSampleRate ppSampleRate;

    switch ((int)options.sampleRate)
    {
        case 48000:
            ppSampleRate = PP_AUDIOSAMPLERATE_48000;
            break;
        default:
            ppSampleRate = PP_AUDIOSAMPLERATE_44100;
            break;
    }

    uint32_t bufferSize =
        pp::AudioConfig::RecommendSampleFrameCount(instance,
                                                   ppSampleRate,
                                                   options.bufferSize > 0 ? options.bufferSize : kDefaultBufferSize);

    pp::AudioConfig audioConfig = pp::AudioConfig(instance,
                                                  ppSampleRate,
                                                  bufferSize);

    m_audio = pp::Audio(instance,
                        audioConfig,
                        processCallback,
                        this);

    m_outputBuffer = std::unique_ptr<MultiChannelBuffer>(
        new MultiChannelBuffer(numOutputs(), m_audio.config().sample_frame_count())
        );

    m_sampleRateRecip = 1. / sampleRate();
}

PepperDriver::~PepperDriver()
{
    // Stop audio unit
    stop();
}

double PepperDriver::sampleRate() const
{
    switch (m_audio.config().sample_rate())
    {
        case PP_AUDIOSAMPLERATE_44100:
            return 44100;
        case PP_AUDIOSAMPLERATE_48000:
            return 48000;
        default:
            throw std::runtime_error("Unknown PP_AudioSampleRate value");
    }
}

size_t PepperDriver::numInputs() const
{
    return 0;
}

size_t PepperDriver::numOutputs() const
{
    return 2;
}

size_t PepperDriver::bufferSize() const
{
    return m_audio.config().sample_frame_count();
}

void PepperDriver::start()
{
    m_audio.StartPlayback();
}

void PepperDriver::stop()
{
    m_audio.StopPlayback();
}

Methcla_Time PepperDriver::currentTime()
{
    const uint64_t frameCount = m_frameCount.load(std::memory_order_relaxed);
    return (double)frameCount * m_sampleRateRecip;
}

void PepperDriver::processCallback(void* samples, uint32_t buffer_size, void* data)
{
    PepperDriver* driver = static_cast<PepperDriver*>(data);

    assert(buffer_size >= driver->m_outputBuffer->numSamples() * sizeof(int16_t));

    // NOTE: Always need to produce the number of frames requested from the browser.
    const size_t numFrames = driver->m_outputBuffer->numFrames();

    driver->process(
        driver->currentTime(),
        numFrames,
        nullptr,
        driver->m_outputBuffer->data()
    );

    driver->m_frameCount.fetch_add(
        numFrames,
        std::memory_order_relaxed
    );

    // Interleave and scale
    Methcla::Audio::interleave(
        static_cast<int16_t*>(samples),
        driver->m_outputBuffer->data(),
        Methcla_AudioSample(std::numeric_limits<int16_t>::max()),
        driver->m_outputBuffer->numChannels(),
        numFrames
    );
}

Driver* Methcla::Platform::defaultAudioDriver(Driver::Options)
{
    throw std::runtime_error("No default audio driver for Pepper");
    return nullptr;
}

Methcla_AudioDriver* methcla_platform_pepper_audio_driver_new(
    const Methcla_AudioDriverOptions* options,
    const pp::InstanceHandle& instance
    )
{
    return Methcla::API::wrapAudioDriver(
        new Methcla::Audio::IO::PepperDriver(
            Methcla::API::convertOptions(options),
            instance
            )
        );
}
