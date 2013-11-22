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

#ifndef METHCLA_AUDIO_MULTICHANNELBUFFER_HPP_INCLUDED
#define METHCLA_AUDIO_MULTICHANNELBUFFER_HPP_INCLUDED

#include "Methcla/Audio.hpp"
#include "Methcla/Memory.hpp"

#include <cstring>

namespace Methcla { namespace Audio {

class MultiChannelBuffer
{
public:
    MultiChannelBuffer(size_t numChannels, size_t numFrames)
        : m_numChannels(numChannels)
        , m_numFrames(numFrames)
        , m_data(makeBuffers(m_numChannels, m_numFrames))
    { }

    ~MultiChannelBuffer()
    {
        freeBuffers(m_numChannels, m_data);
    }

    size_t numChannels() const
    {
        return m_numChannels;
    }

    size_t numFrames() const
    {
        return m_numFrames;
    }

    Methcla_AudioSample* const* data()
    {
        return m_data;
    }

    const Methcla_AudioSample* const* data() const
    {
        return m_data;
    }

    void deinterleave(const Methcla_AudioSample* src, size_t srcFrames)
    {
        Methcla::Audio::deinterleave(data(), src, numChannels(), std::min(numFrames(), srcFrames));
    }

    void deinterleave(const Methcla_AudioSample* src)
    {
        deinterleave(src, numFrames());
    }

    void interleave(Methcla_AudioSample* dst, size_t dstFrames) const
    {
        Methcla::Audio::interleave(dst, data(), numChannels(), std::min(numFrames(), dstFrames));
    }

    void interleave(Methcla_AudioSample* dst) const
    {
        interleave(dst, numFrames());
    }

    void zero()
    {
        for (size_t i=0; i < numChannels(); i++)
        {
            std::memset(data()[i], 0, numFrames() * sizeof(Methcla_AudioSample));
        }
    }

    inline static Methcla_AudioSample** makeBuffers(size_t numChannels, size_t numFrames)
    {
        if (numChannels > 0)
        {
            Methcla_AudioSample** buffers = Methcla::Memory::allocOf<Methcla_AudioSample*>(numChannels);
            for (size_t i=0; i < numChannels; i++)
            {
                buffers[i] = numFrames > 0
                    ? Methcla::Memory::allocAlignedOf<Methcla_AudioSample>(Methcla::Memory::kSIMDAlignment, numFrames)
                    : nullptr;
            }
            return buffers;
        }
        return nullptr;
    }

    inline static void freeBuffers(size_t numChannels, Methcla_AudioSample** buffers)
    {
        if (numChannels > 0 && buffers != nullptr)
        {
            for (size_t i=0; i < numChannels; i++)
            {
                Methcla::Memory::free(buffers[i]);
            }
            Memory::free(buffers);
        }
    }

private:
    size_t                m_numChannels;
    size_t                m_numFrames;
    Methcla_AudioSample** m_data;
};

} }
#endif // METHCLA_AUDIO_MULTICHANNELBUFFER_HPP_INCLUDED

