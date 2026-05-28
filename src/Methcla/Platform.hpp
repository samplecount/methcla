// Copyright (C) 2013 Samplecount S.L.
// Copyright (C) 2026 Methcla contributors
//
// SPDX-License-Identifier: Apache-2.0

#pragma once

#include "Methcla/Audio/IO/Driver.hpp"

#include <methcla/engine.h>

namespace Methcla { namespace Platform {
    Methcla_LogHandler defaultLogHandler();
    Audio::IO::Driver* defaultAudioDriver(Audio::IO::Driver::Options options);
}} // namespace Methcla::Platform
