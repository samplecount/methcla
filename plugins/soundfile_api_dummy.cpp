// Copyright 2013 Samplecount S.L.
//
// Licensed under the Apache License, Version 2.0 (the "License");
// you may not use this file except in compliance with the License.
// You may obtain a copy of the License at
//
//     http://www.apache.org/licenses/LICENSE-2.0
//
// Unless required by applicable law or agreed to in writing, software
// distributed under the License is distributed on an "AS IS" BASIS,
// WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
// See the License for the specific language governing permissions and
// limitations under the License.

#include <methcla/plugins/soundfile_api_dummy.h>
// #include "Log.hpp"

#include <cstdio>
#include <cstring>
#include <iostream>
#include <memory>
#include <random>

#define METHCLA_PRINT_DEBUG(...) fprintf (stderr, __VA_ARGS__)

// Current sound file handle format
enum SoundFileHandleFormat
{
    kSoundFileHandleNoFormat,
    kSoundFileHandleInt16,
    kSoundFileHandleFloat
};

struct SoundFileHandle
{
    int64_t numFrames;
    size_t numChannels;
    double sampleRate;
    SoundFileHandleFormat format;
    int64_t pos;
    Methcla_SoundFile soundFile;
};

extern "C"
{
    static Methcla_Error soundfile_close(const Methcla_SoundFile* file);
    static Methcla_Error soundfile_seek(const Methcla_SoundFile* file, int64_t numFrames);
    static Methcla_Error soundfile_tell(const Methcla_SoundFile* file, int64_t* numFrames);
    static Methcla_Error soundfile_read_float(const Methcla_SoundFile* file, float* buffer, size_t inNumFrames, size_t* outNumFrames);
    static Methcla_Error soundfile_write_float(const Methcla_SoundFile*, const float*, size_t, size_t*);
    static Methcla_Error soundfile_open(const Methcla_SoundFileAPI* api, const char* path, Methcla_FileMode mode, Methcla_SoundFile** outFile, Methcla_SoundFileInfo* info);
} // extern "C"

static Methcla_Error soundfile_close(const Methcla_SoundFile* file)
{
    SoundFileHandle* handle = (SoundFileHandle*)file->handle;
    std::cerr << "soundfile_close " << file << " " << handle << std::endl;
    free(handle);
    return methcla_no_error();
}

static Methcla_Error soundfile_seek(const Methcla_SoundFile* file, int64_t numFrames)
{
    SoundFileHandle* handle = (SoundFileHandle*)file->handle;
    int64_t prevPos = handle->pos;
    handle->pos = std::max<int64_t>(0, std::min(numFrames, handle->numFrames - 1));
    METHCLA_PRINT_DEBUG("soundfile_seek: %p %p prevPos=%lld pos=%lld", file, handle, prevPos, handle->pos);
    return methcla_no_error();
}

static Methcla_Error soundfile_tell(const Methcla_SoundFile* file, int64_t* numFrames)
{
    SoundFileHandle* handle = (SoundFileHandle*)file->handle;
    *numFrames = handle->numFrames;

    return methcla_no_error();
}

static Methcla_Error soundfile_read_float(const Methcla_SoundFile* file, float* buffer, size_t inNumFrames, size_t* outNumFrames)
{
    SoundFileHandle* handle = (SoundFileHandle*)file->handle;

    size_t numFrames = std::min((size_t)(handle->numFrames - handle->pos), std::max<size_t>(0, inNumFrames));
    memset(buffer, 0, numFrames * handle->numChannels * sizeof(float));
    int64_t prevPos = handle->pos;
    handle->pos += numFrames;

    METHCLA_PRINT_DEBUG("soundfile_read_float: %p %p numFrames=%zu prevPos=%lld pos=%lld", file, handle, numFrames, prevPos, handle->pos);

    *outNumFrames = numFrames;

    return methcla_no_error();
}

static Methcla_Error soundfile_write_float(const Methcla_SoundFile* file, const float* buffer, size_t inNumFrames, size_t* outNumFrames)
{
    SoundFileHandle* handle = (SoundFileHandle*)file->handle;

    // TODO: Check the mode file was opened with

    // size_t numFrames = std::min((size_t)(handle->numFrames - handle->pos), std::max<size_t>(0, inNumFrames));
    // memset(buffer, 0, numFrames * handle->numChannels * sizeof(float));
    // int64_t prevPos = handle->pos;
    // handle->pos += numFrames;
    //
    // METHCLA_PRINT_DEBUG("soundfile_read_float: %p %p numFrames=%zu prevPos=%lld pos=%lld", file, handle, numFrames, prevPos, handle->pos);

    *outNumFrames = inNumFrames;

    return methcla_no_error();
}

static Methcla_Error soundfile_open(const Methcla_SoundFileAPI* api, const char* path, Methcla_FileMode mode, Methcla_SoundFile** outFile, Methcla_SoundFileInfo* info)
{
    std::default_random_engine generator;
    std::uniform_int_distribution<int> channelDist(1,2);
    std::uniform_int_distribution<int> framesDist(0.1*44100,10*44100);

    SoundFileHandle* handle = (SoundFileHandle*)malloc(sizeof(SoundFileHandle));
    if (handle == nullptr) {
        return methcla_error_new(kMethcla_MemoryError);
    }

    handle->numFrames = framesDist(generator);
    handle->sampleRate = 44100;
    handle->numChannels = channelDist(generator);
    handle->format = kSoundFileHandleNoFormat;
    handle->pos = 0;

    Methcla_SoundFile* file = &handle->soundFile;
    memset(file, 0, sizeof(*file));
    file->handle = handle;
    file->close = soundfile_close;
    file->seek = soundfile_seek;
    file->tell = soundfile_tell;
    file->read_float = soundfile_read_float;

    *outFile = file;

    info->frames = handle->numFrames;
    info->channels = handle->numChannels;
    info->samplerate = handle->sampleRate;

    METHCLA_PRINT_DEBUG("soundfile_open: %s %lld %u %u", path, info->frames, info->channels, info->samplerate);

    return methcla_no_error();
}

static const Methcla_SoundFileAPI kSoundFileAPI = {
    nullptr,
    nullptr,
    soundfile_open
};

METHCLA_EXPORT const Methcla_Library* methcla_soundfile_api_dummy(const Methcla_Host* host, const char*)
{
    methcla_host_register_soundfile_api(host, &kSoundFileAPI);
    return nullptr;
}
