#include "Mescaline/Audio/IO/JackDriver.hpp"

using namespace Mescaline::Audio::IO;

struct tag_jack_status_t { };
typedef boost::error_info<tag_jack_status_t,jack_status_t> OSStatusInfo;

JackDriver::JackDriver(Client* client) throw (IO::Exception)
    : Driver(client)
{
    // 
    jack_status_t status;
    m_jackClient = jack_client_open("mescaline", JackNullOption, &status);
    if (m_jackClient == 0)
        BOOST_THROW_EXCEPTION(Mescaline::Audio::IO::Exception()
                                << Mescaline::ErrorInfoString("Failed to open jack client"));
}

JackDriver::~JackDriver()
{
}

void JackDriver::start()
{
}

void JackDriver::stop()
{
}
