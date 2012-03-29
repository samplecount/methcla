#include "Mescaline/Audio/IO/JackDriver.hpp"

#include <sstream>

using namespace Mescaline::Audio::IO;
using namespace std;

struct tag_jack_status_t { };
typedef boost::error_info<tag_jack_status_t,jack_status_t> OSStatusInfo;

JackDriver::JackDriver(Client* client) throw (IO::Exception)
    : Driver(client)
    , m_sampleRate(0)
    , m_bufferSize(0)
{
    jack_status_t status;
    m_jackClient = jack_client_open("mescaline", JackNullOption, &status);
    if (m_jackClient == 0)
        BOOST_THROW_EXCEPTION(Mescaline::Audio::IO::Exception()
                                << Mescaline::ErrorInfoString("Failed to open jack client"));

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

    client->configure(*this);
}

JackDriver::~JackDriver()
{
    jack_client_close(m_jackClient);
    delete [] m_inputBuffers;
    delete [] m_outputBuffers;
    delete [] m_jackInputPorts;
    delete [] m_jackOutputPorts;
}

int JackDriver::sampleRateCallback(jack_nframes_t nframes, void* arg)
{
    JackDriver* self = static_cast<JackDriver*>(arg);
    self->m_sampleRate = nframes;
#ifndef NDEBUG
    std::cout << "Mescaline::Audio::IO::JackDriver: the sample rate is now " << nframes << std::endl;
#endif
    return 0;
}

int JackDriver::bufferSizeCallback(jack_nframes_t nframes, void* arg)
{
    JackDriver* self = static_cast<JackDriver*>(arg);
    self->m_bufferSize = nframes;
#ifndef NDEBUG
    std::cout << "Mescaline::Audio::IO::JackDriver: the buffer size is now " << nframes << std::endl;
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
    self->client()->process(nframes, self->m_inputBuffers, self->m_outputBuffers);

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
