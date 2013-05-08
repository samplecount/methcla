/*
    Copyright 2012-2013 Samplecount S.L.

    Licensed under the Apache License, Version 2.0 (the "License");
    you may not use this file except in compliance with the License.
    You may obtain a copy of the License at

        http://www.apache.org/licenses/LICENSE-2.0

    Unless required by applicable law or agreed to in writing, software
    distributed under the License is distributed on an "AS IS" BASIS,
    WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
    See the License for the specific language governing permissions and
    limitations under the License.
*/

#ifndef METHCLA_FILE_H_INCLUDED
#define METHCLA_FILE_H_INCLUDED

#include <methcla/common.h>
#include <stddef.h>
#include <stdint.h>

typedef enum
{
    kMethcla_FileOk
} Methcla_FileError;

// Sound file interface

typedef enum
{
    kMethcla_Read,
    kMethcla_Write,
    kMethcla_ReadWrite
} Methcla_FileMode;

typedef struct
{
    int64_t numFrames;
    unsigned int numChannels;
    unsigned int sampleRate;
} Methcla_SoundFileInfo;

typedef void* Methcla_SoundFileHandle;

typedef struct Methcla_SoundFile
{
    void* handle;
    Methcla_FileError (*close)(struct Methcla_SoundFile* file);
    // Methcla_Error (*error)(Methcla_SoundFileHandle handle);
    int64_t (*seek)(struct Methcla_SoundFile* file, int64_t numFrames, int offset);
    size_t (*read_float)(struct Methcla_SoundFile* file, float* buffer, size_t numFrames);
} Methcla_SoundFile;

typedef struct Methcla_SoundFileAPI
{
    void* handle;
    Methcla_SoundFile* (*open)(const struct Methcla_SoundFileAPI* api, const char* path, Methcla_FileMode mode, Methcla_SoundFileInfo* info);
} Methcla_SoundFileAPI;

static inline Methcla_FileError methcla_soundfile_close(Methcla_SoundFile* file)
{
    return file->close(file);
}

static inline int64_t methcla_soundfile_seek(Methcla_SoundFile* file, int64_t numFrames, int offset)
{
    return file->seek(file, numFrames, offset);
}

static inline size_t methcla_soundfile_read_float(Methcla_SoundFile* file, float* buffer, size_t numFrames)
{
    return file->read_float(file, buffer, numFrames);
}

#endif /* METHCLA_FILE_H_INCLUDED */
