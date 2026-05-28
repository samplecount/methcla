// Copyright (C) 2012-2013 Samplecount S.L.
// Copyright (C) 2026 Methcla contributors
//
// SPDX-License-Identifier: Apache-2.0

#ifndef METHCLA_PLUGINS_SINE_H_INCLUDED
#define METHCLA_PLUGINS_SINE_H_INCLUDED

#include <methcla/plugin.h>

METHCLA_EXPORT Methcla_Library* methcla_plugins_sine(Methcla_Host*,
                                                     const char*);
#define METHCLA_PLUGINS_SINE_URI METHCLA_PLUGINS_URI "/sine"

#endif /* METHCLA_PLUGINS_SINE_H_INCLUDED */
