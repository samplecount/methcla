// Copyright 2013 Samplecount S.L.
//
// SPDX-License-Identifier: Apache-2.0

#ifndef METHCLA_PLUGINS_NODE_CONTROL_H_INCLUDED
#define METHCLA_PLUGINS_NODE_CONTROL_H_INCLUDED

#include <methcla/plugin.h>

METHCLA_EXPORT Methcla_Library* methcla_plugins_node_control(Methcla_Host*,
                                                             const char*);

#define METHCLA_PLUGINS_DONE_AFTER_URI METHCLA_PLUGINS_URI "/done-after"
#define METHCLA_PLUGINS_ASR_ENVELOPE_URI METHCLA_PLUGINS_URI "/asr-envelope"
#define METHCLA_PLUGINS_EXPONENTIAL_FADE_URI \
    METHCLA_PLUGINS_URI "/exponential-fade"

#endif // METHCLA_PLUGINS_NODE_CONTROL_H_INCLUDED
