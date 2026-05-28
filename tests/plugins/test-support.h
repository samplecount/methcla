// Copyright 2016 Samplecount S.L.
//
// SPDX-License-Identifier: Apache-2.0

#ifndef METHCLA_PLUGINS_TEST_SUPPORT_H_INCLUDED
#define METHCLA_PLUGINS_TEST_SUPPORT_H_INCLUDED

#include <methcla/plugin.h>

METHCLA_EXPORT const Methcla_Library*
methcla_plugins_test_support(const Methcla_Host*, const char*);

#define METHCLA_PLUGINS_TEST_STATS_URI METHCLA_PLUGINS_URI "/test/stats"
#define METHCLA_TEST_STATS_OUTPUT_PREFIX "{TEST_STATS}"

#endif // METHCLA_PLUGINS_TEST_SUPPORT_H_INCLUDED
