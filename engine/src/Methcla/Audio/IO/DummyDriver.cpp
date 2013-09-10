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

#include <cassert>

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
}

void DummyDriver::stop()
{
}
