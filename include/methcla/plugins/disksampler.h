// Copyright 2012-2013 Samplecount S.L.
//
// SPDX-License-Identifier: Apache-2.0

#ifndef METHCLA_PLUGINS_DISKSAMPLER_H_INCLUDED
#define METHCLA_PLUGINS_DISKSAMPLER_H_INCLUDED

#include <methcla/plugin.h>

METHCLA_EXPORT Methcla_Library* methcla_plugins_disksampler(Methcla_Host*,
                                                            const char*);
#define METHCLA_PLUGINS_DISKSAMPLER_URI METHCLA_PLUGINS_URI "/disksampler"

#endif // METHCLA_PLUGINS_DISKSAMPLER_H_INCLUDED
