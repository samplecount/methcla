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
    kMethcla_FileNoError,
    kMethcla_FileUnspecifiedError,
    kMethcla_FileInvalidArgument
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
    int64_t      frames;
    unsigned int channels;
    unsigned int samplerate;
} Methcla_SoundFileInfo;

typedef struct Methcla_SoundFile
{
    void* handle;
    Methcla_FileError (*close)(const struct Methcla_SoundFile* file);
    Methcla_FileError (*seek)(const struct Methcla_SoundFile* file, int64_t numFrames);
    Methcla_FileError (*tell)(const struct Methcla_SoundFile* file, int64_t* numFrames);
    Methcla_FileError (*read_float)(const struct Methcla_SoundFile* file, float* buffer, size_t numFrames, size_t* outNumFrames);
} Methcla_SoundFile;

typedef struct Methcla_SoundFileAPI
{
    void* handle;
    const char* (*error_message)(const struct Methcla_SoundFileAPI* api, Methcla_FileError error);
    Methcla_FileError (*open)(const struct Methcla_SoundFileAPI* api, const char* path, Methcla_FileMode mode, Methcla_SoundFile** file, Methcla_SoundFileInfo* info);
} Methcla_SoundFileAPI;

static inline const char* methcla_soundfile_error_message(const Methcla_SoundFileAPI* api, Methcla_FileError error)
{
    if ((api == NULL) || (api->error_message == NULL))
        return "Invalid argument";
    return api->error_message(api, error);
}

static inline Methcla_FileError methcla_soundfile_close(Methcla_SoundFile* file)
{
    if ((file == NULL) || (file->close == NULL))
        return kMethcla_FileInvalidArgument;
    return file->close(file);
}

static inline Methcla_FileError methcla_soundfile_seek(Methcla_SoundFile* file, int64_t numFrames)
{
    if ((file == NULL) || (file->seek == NULL))
        return kMethcla_FileInvalidArgument;
    return file->seek(file, numFrames);
}

static inline Methcla_FileError methcla_soundfile_tell(Methcla_SoundFile* file, int64_t* numFrames)
{
    if ((file == NULL) || (file->tell == NULL) ||
        (numFrames == NULL))
        return kMethcla_FileInvalidArgument;
    return file->tell(file, numFrames);
}

static inline Methcla_FileError methcla_soundfile_read_float(Methcla_SoundFile* file, float* buffer, size_t numFrames, size_t* outNumFrames)
{
    if ((file == NULL) || (file->read_float == NULL) ||
        (buffer == NULL) || (outNumFrames == NULL))
        return kMethcla_FileInvalidArgument;
    return file->read_float(file, buffer, numFrames, outNumFrames);
}

#endif /* METHCLA_FILE_H_INCLUDED */
