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

#ifndef METHCLA_AUDIO_IO_ICECASTDRIVER_HPP
#define METHCLA_AUDIO_IO_ICECASTDRIVER_HPP

#include "Methcla/Audio/IO/Driver.hpp"
#include "Methcla/Audio.hpp"

#include <string>

namespace Methcla { namespace Audio { namespace IO {

    namespace detail
    {
        class IcecastDriverImpl;
    }

    class IcecastDriver : public Driver
    {
    public:
        struct Options
        {
            std::string host = "localhost";
            int port = 8000;
            std::string user = "source";
            std::string password;
            std::string mount;
            std::string name;
            double sampleRate = 44100.;
            size_t bufferSize = 16384;
        };

        IcecastDriver(const Options& options=Options());
        ~IcecastDriver();

        virtual double sampleRate() const override;
        virtual size_t numInputs() const override;
        virtual size_t numOutputs() const override;
        virtual size_t bufferSize() const override;

        virtual void start() override;
        virtual void stop() override;

    private:
        friend class detail::IcecastDriverImpl;
        detail::IcecastDriverImpl* m_impl;
    };

} } }

#endif // METHCLA_AUDIO_IO_ICECASTDRIVER_HPP