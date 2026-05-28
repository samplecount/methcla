// Copyright 2012-2013 Samplecount S.L.
//
// SPDX-License-Identifier: Apache-2.0

#ifndef METHCLA_API_HPP_INCLUDED
#define METHCLA_API_HPP_INCLUDED

#include "Methcla/Audio/Engine.hpp"
#include "Methcla/Audio/IO/Driver.hpp"

#include <methcla/engine.h>

#include <functional>

namespace Methcla { namespace API {
    Methcla::Audio::IO::Driver::Options
    convertOptions(const Methcla_AudioDriverOptions* options);
    Methcla::Audio::Environment::Options
    convertOptions(const Methcla_EngineOptions* options);

    Methcla_AudioDriver* wrapAudioDriver(Methcla::Audio::IO::Driver* driver);
    Methcla::Audio::IO::Driver* getDriver(Methcla_Engine* engine);
}} // namespace Methcla::API

#endif // METHCLA_API_HPP_INCLUDED
