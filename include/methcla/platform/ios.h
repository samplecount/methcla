// Copyright (C) 2014 Samplecount S.L.
// Copyright (C) 2026 Methcla contributors
//
// SPDX-License-Identifier: Apache-2.0

#ifndef METHCLA_PLATFORM_IOS_H_INCLUDED
#define METHCLA_PLATFORM_IOS_H_INCLUDED

#include <methcla/engine.h>

METHCLA_EXPORT Methcla_AudioDriver* methcla_platform_ios_remoteio_driver_new(
    const Methcla_AudioDriverOptions* options);

#endif // METHCLA_PLATFORM_IOS_H_INCLUDED
