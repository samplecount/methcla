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
#include "Methcla/Audio/IO/RtAudioDriver.hpp"
#include "Methcla/API.hpp"
#include "Methcla/Exception.hpp"
#include "Methcla/Memory.hpp"
#include "Methcla/Platform.hpp"

#include <methcla/common.h>
#include <methcla/platform/rtaudio.hpp>

#include <cassert>
#include <cstdint>
#include <limits>
#include <stdexcept>
#include <string>

using namespace Methcla;
using namespace Methcla::Audio::IO;

RtAudioDriver::RtAudioDriver(Options options)
    : Driver(options)
    , m_sampleRate(0)
    , m_isOpen(false)
    , m_isRunning(false)
{
    // Determine the number of devices available
    if (m_audio.getDeviceCount() < 1)
        throw std::runtime_error("RtAudioDriver: No audio devices found");

    RtAudio::StreamParameters iParams, oParams;
    RtAudio::StreamParameters* iParamsPtr = nullptr;

    if (options.numInputs == -1)
    {
        iParams.deviceId = m_audio.getDefaultInputDevice();
        iParams.nChannels = options.numInputs = m_audio.getDeviceInfo(iParams.deviceId).inputChannels;
        iParamsPtr = &iParams;
    }
    else if (options.numInputs > 0)
    {
        iParams.deviceId = m_audio.getDefaultInputDevice();
        iParams.nChannels = options.numInputs;
        iParamsPtr = &iParams;
    }

    if (options.numOutputs == -1)
    {
        oParams.deviceId = m_audio.getDefaultOutputDevice();
        oParams.nChannels = options.numOutputs = m_audio.getDeviceInfo(oParams.deviceId).outputChannels;
    }
    else
    {
        oParams.deviceId = m_audio.getDefaultOutputDevice();
        oParams.nChannels = options.numOutputs;
    }

    const unsigned int sampleRate = options.sampleRate == -1 ? 44100 : options.sampleRate;
    unsigned int bufferFrames = options.bufferSize == -1 ? 0 : options.bufferSize;

    try
    {
        RtAudio::StreamOptions streamOptions;
        streamOptions.flags = RTAUDIO_MINIMIZE_LATENCY | RTAUDIO_SCHEDULE_REALTIME;
        m_audio.openStream(&oParams, iParamsPtr, RTAUDIO_FLOAT32,
                           sampleRate, &bufferFrames, processCallback, this);
        m_sampleRate = m_audio.getStreamSampleRate();
        m_isOpen = true;
    }
    catch (RtError& e)
    {
        throw std::runtime_error(e.what());
    }

    if (iParamsPtr)
    {
        m_inputBuffer = std::unique_ptr<MultiChannelBuffer>(
            new MultiChannelBuffer(iParamsPtr->nChannels, bufferFrames)
        );
    }

    m_outputBuffer = std::unique_ptr<MultiChannelBuffer>(
        new MultiChannelBuffer(oParams.nChannels, bufferFrames)
    );
}

RtAudioDriver::~RtAudioDriver()
{
    stop();
}

double RtAudioDriver::sampleRate() const
{
    return m_sampleRate;
}

size_t RtAudioDriver::numInputs() const
{
    return m_inputBuffer ? m_inputBuffer->numChannels() : 0;
}

size_t RtAudioDriver::numOutputs() const
{
    return m_outputBuffer->numChannels();
}

size_t RtAudioDriver::bufferSize() const
{
    return m_outputBuffer->numFrames();
}

void RtAudioDriver::start()
{
    if (m_isOpen && !m_isRunning) {
        m_audio.startStream();
        m_isRunning = true;
    }
}

void RtAudioDriver::stop()
{
    if (m_isOpen && m_isRunning) {
        m_audio.stopStream();
        m_isRunning = false;
    }
}

Methcla_Time RtAudioDriver::currentTime()
{
    return m_audio.getStreamTime();
}

int RtAudioDriver::processCallback(void* outputBuffer, void* inputBuffer, unsigned int numFrames,
                                   double streamTime, RtAudioStreamStatus, void* data)
{
    static_cast<RtAudioDriver*>(data)->process(
        static_cast<float*>(outputBuffer),
        static_cast<float*>(inputBuffer),
        numFrames,
        streamTime
    );
    return 0;
}

void RtAudioDriver::process(float* outputBuffer, const float* inputBuffer, unsigned int numFrames, double streamTime)
{
    assert(numFrames >= m_outputBuffer->numFrames());

    // Deinterleave input
    if (m_inputBuffer)
        m_inputBuffer->deinterleave(inputBuffer, numFrames);

    Driver::process(
        streamTime,
        m_outputBuffer->numFrames(),
        m_inputBuffer ? m_inputBuffer->data() : nullptr,
        m_outputBuffer->data()
    );
    
    // Interleave output
    m_outputBuffer->interleave(outputBuffer, numFrames);
}

Driver* Methcla::Platform::defaultAudioDriver(Driver::Options options)
{
    return new Methcla::Audio::IO::RtAudioDriver(options);
}

Methcla_AudioDriver* methcla_platform_rtaudio_driver_new(
    const Methcla_AudioDriverOptions* options
    )
{
    return Methcla::API::wrapAudioDriver(
        new Methcla::Audio::IO::RtAudioDriver(
                Methcla::API::convertOptions(options)
            )
        );
}
