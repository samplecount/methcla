#include "Methcla/Audio/IO/Driver.hpp"
#include "Methcla/Memory.hpp"

using namespace Methcla::Audio::IO;
using Methcla::Audio::sample_t;

Driver::Driver()
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

void Driver::process(size_t numFrames, const sample_t* const* inputs, sample_t* const* outputs)
{
    if (m_processCallback != nullptr)
        m_processCallback(m_processData, numFrames, inputs, outputs);
}

sample_t** Driver::makeBuffers(size_t numChannels, size_t numFrames)
{
    sample_t** buffers = Methcla::Memory::allocOf<sample_t*>(numChannels);
    for (size_t i=0; i < numChannels; i++) {
        buffers[i] = Methcla::Memory::allocAlignedOf<sample_t>(Methcla::Memory::kSIMDAlignment, numFrames);
    }
    return buffers;
}
