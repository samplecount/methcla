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

#include <methcla/plugins/soundfile_api_libsndfile.h>
// #include "Log.hpp"

#include <iostream>
#include <memory>
#include <random>

#include <cassert>
#include <cstdlib>
#include <sndfile.h>

struct SoundFileHandle
{
    SNDFILE*          sndfile;
    Methcla_SoundFile soundFile;
};

extern "C" {
    static Methcla_Error soundfile_close(const Methcla_SoundFile*);
    static Methcla_Error soundfile_seek(const Methcla_SoundFile*, int64_t);
    static Methcla_Error soundfile_tell(const Methcla_SoundFile*, int64_t*);
    static Methcla_Error soundfile_read_float(const Methcla_SoundFile*, float*, size_t, size_t*);
    static Methcla_Error soundfile_open(const Methcla_SoundFileAPI*, const char*, Methcla_FileMode, Methcla_SoundFile**, Methcla_SoundFileInfo*);
    static const Methcla_SystemError* soundfile_last_system_error(const Methcla_SoundFileAPI*);
    static const char* soundfile_system_error_description(const Methcla_SystemError*);
    static void soundfile_system_error_destroy(const Methcla_SystemError*);
} // extern "C"

static Methcla_Error soundfile_close(const Methcla_SoundFile* file)
{
    SoundFileHandle* handle = static_cast<SoundFileHandle*>(file->handle);
    sf_close(handle->sndfile);
    free(handle);
    return kMethcla_NoError;
}

static Methcla_Error soundfile_seek(const Methcla_SoundFile* file, int64_t numFrames)
{
    SoundFileHandle* handle = static_cast<SoundFileHandle*>(file->handle);
    sf_count_t n = sf_seek(handle->sndfile, static_cast<sf_count_t>(numFrames), SEEK_SET);
    return n >= 0 ? kMethcla_NoError : kMethcla_UnspecifiedError;
}

static Methcla_Error soundfile_tell(const Methcla_SoundFile* file, int64_t* numFrames)
{
    SoundFileHandle* handle = static_cast<SoundFileHandle*>(file->handle);
    sf_count_t n = sf_seek(handle->sndfile, 0, SEEK_CUR);
    if (n < 0) return kMethcla_UnspecifiedError;
    *numFrames = static_cast<int64_t>(n);
    return kMethcla_NoError;
}

static Methcla_Error soundfile_read_float(const Methcla_SoundFile* file, float* buffer, size_t inNumFrames, size_t* outNumFrames)
{
    SoundFileHandle* handle = static_cast<SoundFileHandle*>(file->handle);

    sf_count_t n = sf_readf_float(handle->sndfile, buffer, inNumFrames);
    if (n < 0) return kMethcla_UnspecifiedError;
    assert(n <= (sf_count_t)inNumFrames);

    *outNumFrames = n;

    return kMethcla_NoError;
}

static bool convertMode(Methcla_FileMode mode, int* outMode)
{
    switch (mode) {
        case kMethcla_FileModeRead:
            *outMode = SFM_READ;
            return true;
        case kMethcla_FileModeWrite:
            *outMode = SFM_WRITE;
            return true;
    }
    return true;
}

static Methcla_Error soundfile_open(const Methcla_SoundFileAPI*, const char* path, Methcla_FileMode mode, Methcla_SoundFile** outFile, Methcla_SoundFileInfo* info)
{
    if (path == nullptr)
        return kMethcla_ArgumentError;
    if (outFile == nullptr)
        return kMethcla_ArgumentError;
    int sfmode;
    if (!convertMode(mode, &sfmode))
        return kMethcla_ArgumentError;

    SoundFileHandle* handle = static_cast<SoundFileHandle*>(std::malloc(sizeof(SoundFileHandle)));
    if (handle == nullptr) {
        return kMethcla_UnspecifiedError;
    }

    SF_INFO sfinfo;
    handle->sndfile = sf_open(path, sfmode, &sfinfo);
    if (handle->sndfile == nullptr)
        return kMethcla_UnspecifiedError;

    Methcla_SoundFile* file = &handle->soundFile;
    memset(file, 0, sizeof(*file));
    file->handle = handle;
    file->close = soundfile_close;
    file->seek = soundfile_seek;
    file->tell = soundfile_tell;
    file->read_float = soundfile_read_float;

    *outFile = file;

    if (info != nullptr) {
        info->frames = sfinfo.frames;
        info->channels = std::abs(sfinfo.channels);
        info->samplerate = std::abs(sfinfo.samplerate);
    }

    // METHCLA_PRINT_DEBUG("soundfile_open: %s %lld %u %u", path, info->frames, info->channels, info->samplerate);

    return kMethcla_NoError;
}

static const char* soundfile_system_error_description(const Methcla_SystemError*)
{
    return "Unknown system error (ExtAudioFile)";
}

static void soundfile_system_error_destroy(const Methcla_SystemError* error)
{
    delete error;
}

static const Methcla_SystemError* soundfile_last_system_error(const Methcla_SoundFileAPI*)
{
    Methcla_SystemError* error = new (std::nothrow) Methcla_SystemError;
    if (error) {
        error->handle = nullptr;
        error->description = soundfile_system_error_description;
        error->destroy = soundfile_system_error_destroy;
    }
    return error;
}

static const Methcla_SoundFileAPI kSoundFileAPI = {
    nullptr,
    soundfile_open,
    soundfile_last_system_error
};

METHCLA_EXPORT const Methcla_Library* methcla_soundfile_api_libsndfile(const Methcla_Host* host, const char*)
{
    methcla_host_register_soundfile_api(host, &kSoundFileAPI);
    return nullptr;
}
