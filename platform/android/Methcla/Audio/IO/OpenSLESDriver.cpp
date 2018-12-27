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

#include "Methcla/Audio/IO/OpenSLESDriver.hpp"
#include "Methcla/Memory.hpp"
#include "Methcla/Platform.hpp"

#include <cassert>
#include <sstream>
#include <stdexcept>

#include <android/log.h>

#define LOGI(...) \
    __android_log_print(ANDROID_LOG_INFO, "OpenSLESDriver", __VA_ARGS__)
#define LOGW(...) \
    __android_log_print(ANDROID_LOG_WARN, "OpenSLESDriver", __VA_ARGS__)

using namespace Methcla::Audio::IO;

OpenSLESDriver::OpenSLESDriver(Options options)
: Driver(options)
, m_stream(nullptr)
, m_sampleRate(options.sampleRate > 0 ? options.sampleRate : 44100)
, m_numInputs(options.numInputs == -1 ? 1 : options.numInputs)
, m_numOutputs(options.numOutputs == -1 ? 2 : options.numOutputs)
, m_bufferSize(options.bufferSize > 0 ? options.bufferSize : kDefaultBufferSize)
, m_inputBuffer(m_numInputs, m_bufferSize)
, m_outputBuffer(m_numOutputs, m_bufferSize)
, m_frameCount(0)
, m_sampleRateRecip(1. / m_sampleRate)
{
    m_stream =
        opensl_open((int)m_sampleRate, (int)m_numInputs, (int)m_numOutputs,
                    (int)m_bufferSize, processCallback, this);

    if (m_stream == nullptr)
    {
        std::stringstream s;
        s << "OpenSLESDriver: Couldn't open audio stream with parameters: "
          << "sampleRate=" << m_sampleRate << " "
          << "numInputs=" << m_numInputs << " "
          << "numOutputs=" << m_numOutputs << " "
          << "bufferSize=" << m_bufferSize;
        throw std::runtime_error(s.str());
    }
}

OpenSLESDriver::~OpenSLESDriver()
{
    if (m_stream != nullptr)
        opensl_close(m_stream);
}

void OpenSLESDriver::start()
{
    if (m_stream != nullptr)
        opensl_start(m_stream);
}

void OpenSLESDriver::stop()
{
    if (m_stream != nullptr)
        opensl_pause(m_stream);
}

Methcla_Time OpenSLESDriver::currentTime()
{
    const uint64_t frameCount = m_frameCount.load(std::memory_order_relaxed);
    return (double)frameCount * m_sampleRateRecip;
}

void OpenSLESDriver::processCallback(void* context, int sample_rate,
                                     int buffer_frames, int input_channels,
                                     const short* input_buffer,
                                     int output_channels, short* output_buffer)
{
    OpenSLESDriver* driver = static_cast<OpenSLESDriver*>(context);

    // const size_t numInputs = self->m_numInputs;
    // const size_t numOutputs = self->m_numOutputs;
    // const size_t numFrames = (size_t)buffer_frames;

    assert(driver->m_sampleRate == (double)sample_rate);
    assert(driver->m_inputBuffer.numChannels() == (size_t)input_channels);
    assert(driver->m_outputBuffer.numChannels() == (size_t)output_channels);
    assert(buffer_frames >= 0 && (size_t)buffer_frames <= driver->bufferSize());
    static_assert(sizeof(short) == sizeof(int16_t), "OOPS");

    // sample_t** inputBuffers = self->m_inputBuffers;
    // sample_t** outputBuffers = self->m_outputBuffer;
    //
    // // Deinterleave and convert input
    // for (size_t curChan = 0; curChan < numInputs; curChan++) {
    //     for (size_t curFrame = 0; curFrame < numFrames; curFrame++) {
    //         inputBuffers[curChan][curFrame] = input_buffer[curFrame *
    //         numInputs + curChan] / 32768.f;
    //     }
    // }

    // assert(buffer_size >= driver->m_outputBuffer->numSamples());

    Methcla::Audio::deinterleave(
        driver->m_inputBuffer.data(), static_cast<const int16_t*>(input_buffer),
        1.f / (float)Methcla_AudioSample(std::numeric_limits<int16_t>::max()),
        driver->m_inputBuffer.numChannels(), buffer_frames);

    driver->process(driver->currentTime(), buffer_frames,
                    driver->m_inputBuffer.data(),
                    driver->m_outputBuffer.data());

    driver->m_frameCount.fetch_add(buffer_frames, std::memory_order_relaxed);

    // Interleave and scale
    Methcla::Audio::interleave(
        static_cast<int16_t*>(output_buffer), driver->m_outputBuffer.data(),
        Methcla_AudioSample(std::numeric_limits<int16_t>::max()),
        driver->m_outputBuffer.numChannels(), buffer_frames);

    //     // Run DSP graph
    //     try {
    //         self->process(numFrames, inputBuffers, outputBuffers);
    //     } catch (std::exception& e) {
    //         LOGW(e.what());
    //     }
    // #ifndef NDEBUG
    //     catch (...) {
    //         LOGW("Unknown exception caught");
    //     }
    // #endif

    // // Convert and interleave output
    // for (size_t curChan = 0; curChan < numOutputs; curChan++) {
    //     for (size_t curFrame = 0; curFrame < numFrames; curFrame++) {
    //         output_buffer[curFrame * numOutputs + curChan] =
    //         outputBuffers[curChan][curFrame] * 32767.f;
    //     }
    // }
}

Driver* Methcla::Platform::defaultAudioDriver(Driver::Options options)
{
    return new OpenSLESDriver(options);
}
