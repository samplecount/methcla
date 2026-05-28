// Copyright 2012-2013 Samplecount S.L.
//
// SPDX-License-Identifier: Apache-2.0

#ifndef METHCLA_AUDIO_HPP_INCLUDED
#define METHCLA_AUDIO_HPP_INCLUDED

#include <methcla/common.h>
#include <methcla/engine.h>

#include <cstdint>

namespace Methcla { namespace Audio {
    typedef uint32_t            Epoch;
    typedef Methcla_AudioSample sample_t;

    class TimeInterface
    {
        virtual Methcla_Time currentTime() = 0;
    };

    template <typename A, typename B>
    void deinterleave(A* const* dst, const B* src, size_t numChannels,
                      size_t numFrames)
    {
        for (size_t c = 0; c < numChannels; c++)
        {
            for (size_t i = 0; i < numFrames; i++)
            {
                dst[c][i] = src[i * numChannels + c];
            }
        }
    }

    template <typename A, typename B>
    void deinterleave(A* const* dst, const B* src, A scale, size_t numChannels,
                      size_t numFrames)
    {
        for (size_t c = 0; c < numChannels; c++)
        {
            for (size_t i = 0; i < numFrames; i++)
            {
                dst[c][i] = scale * static_cast<A>(src[i * numChannels + c]);
            }
        }
    }

    template <typename A, typename B>
    void interleave(A* dst, const B* const* src, size_t numChannels,
                    size_t numFrames)
    {
        for (size_t c = 0; c < numChannels; c++)
        {
            for (size_t i = 0; i < numFrames; i++)
            {
                dst[i * numChannels + c] = src[c][i];
            }
        }
    }

    template <typename A, typename B>
    void interleave(A* dst, const B* const* src, B scale, size_t numChannels,
                    size_t numFrames)
    {
        for (size_t c = 0; c < numChannels; c++)
        {
            for (size_t i = 0; i < numFrames; i++)
            {
                dst[i * numChannels + c] = static_cast<A>(scale * src[c][i]);
            }
        }
    }
}} // namespace Methcla::Audio

#endif // METHCLA_AUDIO_HPP_INCLUDED
