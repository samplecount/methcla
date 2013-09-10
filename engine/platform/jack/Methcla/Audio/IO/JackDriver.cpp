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

#include "Methcla/Audio/IO/JackDriver.hpp"
#include "Methcla/Exception.hpp"
#include "Methcla/Memory.hpp"

#include <methcla/common.h>

#include <cassert>
#include <sstream>

using namespace Methcla;
using namespace Methcla::Audio::IO;
using Methcla::Audio::sample_t;
using namespace std;

JackDriver::JackDriver()
    : m_sampleRate(0)
    , m_bufferSize(0)
{
    jack_status_t status;

    m_jackClient = jack_client_open("methcla", JackNullOption, &status);
    if (m_jackClient == nullptr) {
        throw Error(kMethcla_DeviceUnavailableError);
    }

    m_sampleRate = jack_get_sample_rate(m_jackClient);
    m_bufferSize = jack_get_buffer_size(m_jackClient);

    static const size_t kNumPorts = 2;
    m_numInputs = kNumPorts;
    m_numOutputs = kNumPorts;

    m_inputBuffers = new sample_t*[m_numInputs];
    m_outputBuffers = new sample_t*[m_numOutputs];

    m_jackInputPorts = new jack_port_t*[m_numInputs];
    m_jackOutputPorts = new jack_port_t*[m_numOutputs];

    for (size_t i=0; i < kNumPorts; i++) {
        ostringstream inputName;
        inputName << "input_" << i;
        m_jackInputPorts[i] =
            jack_port_register( m_jackClient
                              , inputName.str().c_str()
                              , JACK_DEFAULT_AUDIO_TYPE
                              , JackPortIsInput
                              , 0
                              );
        ostringstream outputName;
        outputName << "output_" << i;
        m_jackOutputPorts[i] =
            jack_port_register( m_jackClient
                              , outputName.str().c_str()
                              , JACK_DEFAULT_AUDIO_TYPE
                              , JackPortIsOutput
                              , 0
                              );
    }

    jack_set_process_callback(m_jackClient, processCallback, this);
    jack_set_sample_rate_callback(m_jackClient, sampleRateCallback, this);
    jack_set_buffer_size_callback(m_jackClient, bufferSizeCallback, this);
}

JackDriver::~JackDriver()
{
    BOOST_VERIFY(jack_deactivate(m_jackClient) == 0);
    delete [] m_inputBuffers;
    delete [] m_outputBuffers;

    for (size_t i=0; i < numInputs(); i++) {
        BOOST_VERIFY(jack_port_unregister(m_jackClient, m_jackInputPorts[i]) == 0);
    }
    delete [] m_jackInputPorts;

    for (size_t i=0; i < numOutputs(); i++) {
        BOOST_VERIFY(jack_port_unregister(m_jackClient, m_jackOutputPorts[i]) == 0);
    }
    delete [] m_jackOutputPorts;

    BOOST_VERIFY(jack_client_close(m_jackClient) == 0);
}

int JackDriver::sampleRateCallback(jack_nframes_t nframes, void* arg)
{
    JackDriver* self = static_cast<JackDriver*>(arg);
    self->m_sampleRate = nframes;
#ifndef NDEBUG
    std::cout << "Methcla::Audio::IO::JackDriver: the sample rate is now " << nframes << std::endl;
#endif
    return 0;
}

int JackDriver::bufferSizeCallback(jack_nframes_t nframes, void* arg)
{
    JackDriver* self = static_cast<JackDriver*>(arg);
    self->m_bufferSize = nframes;
#ifndef NDEBUG
    std::cout << "Methcla::Audio::IO::JackDriver: the buffer size is now " << nframes << std::endl;
#endif
    return 0;
}

int JackDriver::processCallback(jack_nframes_t nframes, void* arg)
{
    JackDriver* self = static_cast<JackDriver*>(arg);
    const size_t numInputs = self->numInputs();
    const size_t numOutputs = self->numOutputs();

    for (size_t i=0; i < numInputs; i++) {
        self->m_inputBuffers[i] = static_cast<sample_t*>(
            jack_port_get_buffer(self->m_jackInputPorts[i], nframes) );
    }
    for (size_t i=0; i < numOutputs; i++) {
        self->m_outputBuffers[i] = static_cast<sample_t*>(
            jack_port_get_buffer(self->m_jackOutputPorts[i], nframes) );
    }

    // for (size_t i=0; i < min(numInputs,numOutputs); i++) {
    //     memcpy(self->m_outputBuffers[i], self->m_inputBuffers[i], sizeof(sample_t) * nframes);
    // }

    // Run DSP graph
    self->process(nframes, self->m_inputBuffers, self->m_outputBuffers);

    return 0;
}

void JackDriver::start()
{
    jack_activate(m_jackClient);
}

void JackDriver::stop()
{
    jack_deactivate(m_jackClient);
}

Driver* Methcla::Audio::IO::Options()
{
    return new JackDriver();
}
