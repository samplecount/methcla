#include "Methcla/Audio/IO/Driver.hpp"
#include <memory>

using namespace Methcla::Audio::IO;

Driver::Driver()
{
    setProcessCallback(nullptr, nullptr);
}

Driver::~Driver()
{
}

void Driver::setProcessCallback(ProcessCallback callback, void* data)
{
    m_process.callback = callback;
    m_process.data = data;
}

void Driver::process(size_t numFrames, sample_t** inputs, sample_t** outputs)
{
    if (m_process.callback != nullptr) m_process.callback(m_process.data, numFrames, inputs, outputs);
}
