#include "Methcla/Audio/IO/Driver.hpp"

using namespace Methcla::Audio::IO;
using Methcla::Audio::sample_t;

Driver::Driver(Options options)
    : m_blockSize(options.blockSize >= 0 ? std::max(1, options.blockSize) : 64)
{
    setProcessCallback(nullptr, nullptr);
}

Driver::~Driver()
{
}

size_t Driver::blockSize() const
{
    return m_blockSize;
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

Methcla_Time Driver::currentTime() const
{
    return 0.;
}
