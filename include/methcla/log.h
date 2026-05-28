// Copyright (C) 2012-2013 Samplecount S.L.
// Copyright (C) 2026 Methcla contributors
//
// SPDX-License-Identifier: Apache-2.0

#pragma once

typedef enum Methcla_LogLevel
{
    kMethcla_LogError,
    kMethcla_LogWarn,
    kMethcla_LogInfo,
    kMethcla_LogDebug
} Methcla_LogLevel;

typedef struct Methcla_LogHandler
{
    void* handle;
    void (*log_line)(void* handle, Methcla_LogLevel level, const char* message);
} Methcla_LogHandler;
