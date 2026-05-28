// Copyright (C) 2012-2013 Samplecount S.L.
// Copyright (C) 2026 Methcla contributors
//
// SPDX-License-Identifier: Apache-2.0

#pragma once

#include "Methcla/Audio.hpp"
#include "Methcla/Audio/MultiChannelBuffer.hpp"

#include <cstdint>
#include <cstring>
#include <optional>

namespace Methcla { namespace Audio { namespace IO {

    class Driver : public Methcla::Audio::TimeInterface
    {
    public:
        static constexpr size_t kDefaultBufferSize = 512;

        struct Options
        {
            std::optional<double> sampleRate;
            std::optional<size_t> numInputs;
            std::optional<size_t> numOutputs;
            std::optional<size_t> bufferSize;
        };

        typedef void (*ProcessCallback)(void* data, Methcla_Time currentTime,
                                        size_t                 numFrames,
                                        const sample_t* const* inputs,
                                        sample_t* const*       outputs);

        Driver(Options options);
        virtual ~Driver();

        void setProcessCallback(ProcessCallback callback, void* data);

        virtual double sampleRate() const = 0;
        virtual size_t numInputs() const = 0;
        virtual size_t numOutputs() const = 0;
        virtual size_t bufferSize() const = 0;

        virtual Methcla_Time currentTime();

        virtual void start() = 0;
        virtual void stop() = 0;

    protected:
        void process(Methcla_Time currentTime, size_t numFrames,
                     const sample_t* const* inputs, sample_t* const* outputs);

    private:
        ProcessCallback m_processCallback;
        void*           m_processData;
    };

}}} // namespace Methcla::Audio::IO
