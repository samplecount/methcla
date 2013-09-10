#include "Methcla/Audio/IO/Driver.hpp"
#include "Methcla/Memory.hpp"

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

void Driver::process(size_t numFrames, const sample_t* const* inputs, sample_t* const* outputs)
{
    if (m_processCallback != nullptr)
        m_processCallback(m_processData, numFrames, inputs, outputs);
}

sample_t** Driver::makeBuffers(size_t numChannels, size_t numFrames)
{
    if (numChannels > 0) {
        sample_t** buffers = Methcla::Memory::allocOf<sample_t*>(numChannels);
        for (size_t i=0; i < numChannels; i++) {
            buffers[i] = numFrames > 0
                ? Methcla::Memory::allocAlignedOf<sample_t>(Methcla::Memory::kSIMDAlignment, numFrames)
                : nullptr;
        }
        return buffers;
    }
    return nullptr;
}

void Driver::freeBuffers(size_t numChannels, sample_t** buffers)
{
    if (numChannels > 0 && buffers != nullptr) {
        for (size_t i=0; i < numChannels; i++) {
            Methcla::Memory::free(buffers[i]);
        }
        Memory::free(buffers);
    }
}
