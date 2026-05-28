// Copyright (C) 2012-2013 Samplecount S.L.
// Copyright (C) 2026 Methcla contributors
//
// SPDX-License-Identifier: Apache-2.0

#pragma once

#include <methcla/common.h>

#include <stddef.h>
#include <stdint.h>

#if defined(__cplusplus)
extern "C" {
#endif

typedef enum
{
    kMethcla_FileModeRead,
    kMethcla_FileModeWrite
} Methcla_FileMode;

typedef enum
{
    kMethcla_SoundFileTypeUnknown,
    kMethcla_SoundFileTypeAIFF,
    kMethcla_SoundFileTypeWAV
} Methcla_SoundFileType;

typedef enum
{
    kMethcla_SoundFileFormatUnknown,
    kMethcla_SoundFileFormatPCM16,
    kMethcla_SoundFileFormatPCM24,
    kMethcla_SoundFileFormatPCM32,
    kMethcla_SoundFileFormatFloat
} Methcla_SoundFileFormat;

typedef struct
{
    int64_t                 frames;
    unsigned int            channels;
    unsigned int            samplerate;
    Methcla_SoundFileType   file_type;
    Methcla_SoundFileFormat file_format;
} Methcla_SoundFileInfo;

typedef struct Methcla_SoundFile Methcla_SoundFile;

struct Methcla_SoundFile
{
    void* handle;
    Methcla_Error (*close)(Methcla_SoundFile* file);
    Methcla_Error (*seek)(Methcla_SoundFile* file, int64_t numFrames);
    Methcla_Error (*tell)(Methcla_SoundFile* file, int64_t* numFrames);
    Methcla_Error (*read_float)(Methcla_SoundFile* file, float* buffer,
                                size_t numFrames, size_t* outNumFrames);
    Methcla_Error (*write_float)(Methcla_SoundFile* file, const float* buffer,
                                 size_t numFrames, size_t* outNumFrames);
};

typedef struct Methcla_SoundFileAPI Methcla_SoundFileAPI;

struct Methcla_SoundFileAPI
{
    void*       handle;
    const char* valid_file_extensions;
    Methcla_Error (*open)(Methcla_SoundFileAPI* api, const char* path,
                          Methcla_FileMode mode, Methcla_SoundFile** file,
                          Methcla_SoundFileInfo* info);
};

static inline Methcla_Error methcla_soundfile_close(Methcla_SoundFile* file)
{
    if ((file == NULL) || (file->close == NULL))
        return methcla_error_new(kMethcla_ArgumentError);
    return file->close(file);
}

static inline Methcla_Error methcla_soundfile_seek(Methcla_SoundFile* file,
                                                   int64_t            numFrames)
{
    if ((file == NULL) || (file->seek == NULL))
        return methcla_error_new(kMethcla_ArgumentError);
    return file->seek(file, numFrames);
}

static inline Methcla_Error methcla_soundfile_tell(Methcla_SoundFile* file,
                                                   int64_t*           numFrames)
{
    if ((file == NULL) || (file->tell == NULL) || (numFrames == NULL))
        return methcla_error_new(kMethcla_ArgumentError);
    return file->tell(file, numFrames);
}

static inline Methcla_Error
methcla_soundfile_read_float(Methcla_SoundFile* file, float* buffer,
                             size_t numFrames, size_t* outNumFrames)
{
    if ((file == NULL) || (file->read_float == NULL) || (buffer == NULL) ||
        (outNumFrames == NULL))
        return methcla_error_new(kMethcla_ArgumentError);
    return file->read_float(file, buffer, numFrames, outNumFrames);
}

static inline Methcla_Error
methcla_soundfile_write_float(Methcla_SoundFile* file, const float* buffer,
                              size_t numFrames, size_t* outNumFrames)
{
    if ((file == NULL) || (file->write_float == NULL) || (buffer == NULL) ||
        (outNumFrames == NULL))
        return methcla_error_new(kMethcla_ArgumentError);
    return file->write_float(file, buffer, numFrames, outNumFrames);
}

#if defined(__cplusplus)
}
#endif
