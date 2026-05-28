// Copyright 2012-2013 Samplecount S.L.
//
// SPDX-License-Identifier: Apache-2.0

#ifndef METHCLA_LOG_H_INCLUDED
#define METHCLA_LOG_H_INCLUDED

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

#endif /* METHCLA_LOG_H_INCLUDED */
