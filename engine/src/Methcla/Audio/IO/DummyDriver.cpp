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

#include "Methcla/Audio/IO/DummyDriver.hpp"
#include "Methcla/Platform.hpp"

#include <cassert>
#include <cstring>
#include <iostream>

#if defined(__native_client__)
#  include <time.h>
#else
#  include <chrono>
#endif

using namespace Methcla::Audio::IO;

DummyDriver::DummyDriver(Options options)
    : Driver(options)
    , m_sampleRate(options.sampleRate >= 0 ? options.sampleRate : kDefaultSampleRate)
    , m_numInputs(options.numInputs >= 0 ? options.numInputs : kDefaultNumInputs)
    , m_numOutputs(options.numOutputs >= 0 ? options.numOutputs : kDefaultNumOutputs)
    , m_bufferSize(options.bufferSize >= 0 ? options.bufferSize : kDefaultBufferSize)
{
    assert(m_sampleRate > 0);
    assert(m_numOutputs > 0);
    assert(m_bufferSize > 0);
    assert(m_time.is_lock_free());
    m_inputBuffers = makeBuffers(m_numInputs, m_bufferSize);
    m_outputBuffers = makeBuffers(m_numOutputs, m_bufferSize);
}

DummyDriver::~DummyDriver()
{
    freeBuffers(m_numInputs, m_inputBuffers);
    freeBuffers(m_numOutputs, m_outputBuffers);
}

void DummyDriver::start()
{
    if (!m_thread.joinable())
    {
        m_continue = true;
        m_thread = std::thread(&DummyDriver::run, this);
    }
}

void DummyDriver::stop()
{
    if (m_thread.joinable())
    {
        m_continue = false;
        m_thread.join();
    }
}

static void storeTime(std::atomic<uint64_t>& mem, double time)
{
    static_assert(sizeof(uint64_t) == sizeof(double), "double not 64 bit");
    uint64_t t64;
    std::memcpy(&t64, &time, sizeof(t64));
    mem.store(t64);
}

void DummyDriver::run()
{
#if defined(__native_client__)
    // TODO: Remove drift and jitter
    const double dt = (double)bufferSize()/(double)sampleRate();
    double t = 0.;
    while (m_continue)
    {
        storeTime(m_time, t);
        process(t, bufferSize(), m_inputBuffers, m_outputBuffers);
        struct timespec ts;
        ts.tv_sec = dt;
        ts.tv_nsec = (dt - ts.tv_sec) * 1e9;
        nanosleep(&ts, &ts);
        t += dt;
    }
#else
    auto dt = std::chrono::duration<double>(bufferSize()/sampleRate());
    auto t0 = std::chrono::steady_clock::now();
    auto t = t0 + std::chrono::duration<double>(0);

    while (m_continue)
    {
        const double tDouble = std::chrono::duration_cast<std::chrono::duration<double>>(t-t0).count();
        storeTime(m_time, tDouble);
        process(tDouble, bufferSize(), m_inputBuffers, m_outputBuffers);
        std::this_thread::sleep_until(t);
        t += dt;
    }
#endif
}

Methcla_Time DummyDriver::currentTime() const
{
    const uint64_t t64 = m_time.load();
    double result;
    std::memcpy(&result, &t64, sizeof(t64));
    return result;
}

Driver* Methcla::Platform::defaultAudioDriver(Driver::Options options)
{
    return new DummyDriver(options);
}
