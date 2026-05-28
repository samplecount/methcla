// Copyright 2012-2013 Samplecount S.L.
//
// SPDX-License-Identifier: Apache-2.0

#ifndef METHCLA_PLUGINS_PATCH_CABLE_H_INCLUDED
#define METHCLA_PLUGINS_PATCH_CABLE_H_INCLUDED

#include <methcla/plugin.h>

METHCLA_EXPORT Methcla_Library* methcla_plugins_patch_cable(Methcla_Host*,
                                                            const char*);
#define METHCLA_PLUGINS_PATCH_CABLE_URI METHCLA_PLUGINS_URI "/patch-cable"

#define METHCLA_PLUGINS_AMPLIFIER_URI METHCLA_PLUGINS_URI "/amplifier"

#endif // METHCLA_PLUGINS_PATCH_CABLE_H_INCLUDED
