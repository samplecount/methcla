// Copyright (C) 2012-2013 Samplecount S.L.
// Copyright (C) 2026 Methcla contributors
//
// SPDX-License-Identifier: Apache-2.0

#pragma once

#include <methcla/plugin.h>

METHCLA_EXPORT Methcla_Library* methcla_plugins_sampler(Methcla_Host*,
                                                        const char*);
#define METHCLA_PLUGINS_SAMPLER_URI METHCLA_PLUGINS_URI "/sampler"
