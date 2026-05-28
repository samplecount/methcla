// Copyright (C) 2012-2013 Samplecount S.L.
// Copyright (C) 2026 Methcla contributors
//
// SPDX-License-Identifier: Apache-2.0

#pragma once

#include <methcla/plugin.h>

METHCLA_EXPORT Methcla_Library* methcla_plugins_patch_cable(Methcla_Host*,
                                                            const char*);
#define METHCLA_PLUGINS_PATCH_CABLE_URI METHCLA_PLUGINS_URI "/patch-cable"

#define METHCLA_PLUGINS_AMPLIFIER_URI METHCLA_PLUGINS_URI "/amplifier"
