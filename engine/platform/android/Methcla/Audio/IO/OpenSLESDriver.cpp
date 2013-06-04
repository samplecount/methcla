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
#include "opensl_io.h"

#include <android/log.h>
#include <cassert>
#include <stdexcept>

#define LOGI(...) \
  __android_log_print(ANDROID_LOG_INFO, "OpenSLESDriver", __VA_ARGS__)
#define LOGW(...) \
  __android_log_print(ANDROID_LOG_WARN, "OpenSLESDriver", __VA_ARGS__)

using namespace Methcla::Audio::IO;

OpenSLESDriver::OpenSLESDriver()
    : m_stream(nullptr)
    , m_sampleRate(44100)
    , m_numInputs(0)
    , m_numOutputs(2)
    , m_bufferSize(512)
    , m_inputBuffers(nullptr)
    , m_outputBuffers(nullptr)
{
    m_stream = opensl_open(
        (int)m_sampleRate,
        (int)m_numInputs,
        (int)m_numOutputs,
        (int)m_bufferSize,
        processCallback,
        this
    );
    if (m_stream == nullptr) {
        throw std::runtime_error("OpenSLESDriver: Couldn't open audio stream");
    }

    m_inputBuffers = makeBuffers(m_numInputs, m_bufferSize);
    m_outputBuffers = makeBuffers(m_numOutputs, m_bufferSize);
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

void OpenSLESDriver::processCallback(
    void* context, int sample_rate, int buffer_frames,
    int input_channels, const short* input_buffer,
    int output_channels, short* output_buffer)
{
    OpenSLESDriver* self = static_cast<OpenSLESDriver*>(context);

    const size_t numInputs = self->m_numInputs;
    const size_t numOutputs = self->m_numOutputs;
    const size_t bufferSize = self->m_bufferSize;
    const size_t numFrames = (size_t)buffer_frames;

    assert( self->m_sampleRate == (double)sample_rate );
    assert( numInputs == (size_t)input_channels );
    assert( numOutputs == (size_t)output_channels );
    assert( buffer_frames >= 0 && bufferSize <= (size_t)buffer_frames );

    sample_t** inputBuffers = self->m_inputBuffers;
    sample_t** outputBuffers = self->m_outputBuffers;

    // Deinterleave and convert input
    for (size_t curChan = 0; curChan < numInputs; curChan++) {
        for (size_t curFrame = 0; curFrame < numFrames; curFrame++) {
            inputBuffers[curChan][curFrame] = input_buffer[curFrame * numInputs + curChan] / 32768.f;
        }
    }

    // Run DSP graph
    try {
        self->process(numFrames, inputBuffers, outputBuffers);
    } catch (std::exception& e) {
        LOGW(e.what());
    }
#ifndef NDEBUG
    catch (...) {
        LOGW("Unknown exception caught");
    }
#endif

    // Convert and interleave output
    for (size_t curChan = 0; curChan < numOutputs; curChan++) {
        for (size_t curFrame = 0; curFrame < numFrames; curFrame++) {
            output_buffer[curFrame * numOutputs + curChan] = outputBuffers[curChan][curFrame] * 32767.f;
        }
    }
}

Driver* Methcla::Audio::IO::defaultPlatformDriver()
{
    return new OpenSLESDriver();
}
