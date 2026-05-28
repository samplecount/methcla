// Copyright (C) 2012-2013 Samplecount S.L.
// Copyright (C) 2026 Methcla contributors
//
// SPDX-License-Identifier: Apache-2.0

#pragma once

#if defined(__GNUC__)
#    if __GNUC__ > 4
#        define METHCLA_WITHOUT_WARNINGS_BEGIN                   \
            _Pragma("GCC diagnostic push") _Pragma(              \
                "GCC diagnostic ignored \"-Wunused-parameter\"") \
                _Pragma("GCC diagnostic ignored \"-Wunused-private-field\"")
#        define METHCLA_WITHOUT_WARNINGS_END _Pragma("GCC diagnostic pop")
#    else
#        define METHCLA_WITHOUT_WARNINGS_BEGIN \
            _Pragma("GCC diagnostic push")     \
                _Pragma("GCC diagnostic ignored \"-Wunused-parameter\"")
#        define METHCLA_WITHOUT_WARNINGS_END _Pragma("GCC diagnostic pop")
#    endif
#else
#    define METHCLA_WITHOUT_WARNINGS_BEGIN
#    define METHCLA_WITHOUT_WARNINGS_END
#endif
