// Copyright 2013 Samplecount S.L.
//
// SPDX-License-Identifier: Apache-2.0

#ifndef METHCLA_PLATFORM_HPP_INCLUDED
#define METHCLA_PLATFORM_HPP_INCLUDED

#include "Methcla/Audio/IO/Driver.hpp"

#include <methcla/engine.h>

namespace Methcla { namespace Platform {
    Methcla_LogHandler defaultLogHandler();
    Audio::IO::Driver* defaultAudioDriver(Audio::IO::Driver::Options options);
}} // namespace Methcla::Platform

#endif // METHCLA_PLATFORM_HPP_INCLUDED
