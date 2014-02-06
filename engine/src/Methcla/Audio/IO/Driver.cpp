#include "Methcla/Audio/IO/Driver.hpp"

using namespace Methcla::Audio::IO;
using Methcla::Audio::sample_t;

Driver::Driver(Options)
{
    setProcessCallback(nullptr, nullptr);
}

Driver::~Driver()
{
}

void Driver::setProcessCallback(ProcessCallback callback, void* data)
{
    m_processCallback = callback;
    m_processData = data;
}

void Driver::process(Methcla_Time currentTime, size_t numFrames, const sample_t* const* inputs, sample_t* const* outputs)
{
    if (m_processCallback != nullptr)
        m_processCallback(m_processData, currentTime, numFrames, inputs, outputs);
}

Methcla_Time Driver::currentTime()
{
    return 0.;
}
