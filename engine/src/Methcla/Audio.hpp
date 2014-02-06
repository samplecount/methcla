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

#ifndef METHCLA_AUDIO_HPP_INCLUDED
#define METHCLA_AUDIO_HPP_INCLUDED

#include <cstdint>
#include <methcla/common.h>
#include <methcla/engine.h>

namespace Methcla { namespace Audio
{
    typedef uint32_t Epoch;
    typedef Methcla_AudioSample sample_t;

    class TimeInterface
    {
        virtual Methcla_Time currentTime() = 0;
    };

    template <typename A, typename B> void deinterleave(A* const* dst, const B* src, size_t numChannels, size_t numFrames)
    {
        for (size_t c=0; c < numChannels; c++)
        {
            for (size_t i=0; i < numFrames; i++)
            {
                dst[c][i] = src[i*numChannels+c];
            }
        }
    }

    template <typename A, typename B> void interleave(A* dst, const B* const* src, size_t numChannels, size_t numFrames)
    {
        for (size_t c=0; c < numChannels; c++)
        {
            for (size_t i=0; i < numFrames; i++)
            {
                dst[i*numChannels+c] = src[c][i];
            }
        }
    }

    template <typename A, typename B> void interleave(A* dst, const B* const* src, B scale, size_t numChannels, size_t numFrames)
    {
        for (size_t c=0; c < numChannels; c++)
        {
            for (size_t i=0; i < numFrames; i++)
            {
                dst[i*numChannels+c] = static_cast<A>(scale * src[c][i]);
            }
        }
    }
} }

#endif // METHCLA_AUDIO_HPP_INCLUDED
