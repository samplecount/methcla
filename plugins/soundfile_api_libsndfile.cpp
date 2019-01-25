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

#include <cassert>
#include <cstdlib>
#include <cstring>
#include <iostream>
#include <memory>
#include <random>

#include <sndfile.h>

namespace {
    struct SoundFileHandle
    {
        SNDFILE*          sndfile;
        Methcla_SoundFile soundFile;

        struct Destructor
        {
            void operator()(SoundFileHandle* handle)
            {
                if (handle != nullptr)
                {
                    if (handle->sndfile != nullptr)
                        sf_close(handle->sndfile);
                    free(handle);
                }
            }
        };

        typedef std::unique_ptr<SoundFileHandle, Destructor> Ref;

        Methcla_Error error()
        {
            if (sndfile == nullptr)
            {
                return methcla_error_new_with_message(kMethcla_UnspecifiedError,
                                                      sf_strerror(nullptr));
            }
            else
            {
                int code = sf_error(sndfile);
                if (code == SF_ERR_NO_ERROR)
                    return methcla_no_error();
                return methcla_error_new_with_message(convertErrorCode(code),
                                                      sf_strerror(sndfile));
            }
        }

    private:
        Methcla_ErrorCode convertErrorCode(int sfcode)
        {
            switch (sfcode)
            {
                case SF_ERR_UNRECOGNISED_FORMAT:
                    return kMethcla_UnsupportedDataFormatError;
                case SF_ERR_SYSTEM:
                    return kMethcla_SystemError;
                case SF_ERR_MALFORMED_FILE:
                    return kMethcla_InvalidFileError;
                case SF_ERR_UNSUPPORTED_ENCODING:
                    return kMethcla_UnsupportedFileTypeError;
            }

            return kMethcla_UnspecifiedError;
        }
    };

    bool convertMode(Methcla_FileMode mode, int* outMode)
    {
        switch (mode)
        {
            case kMethcla_FileModeRead:
                *outMode = SFM_READ;
                return true;
            case kMethcla_FileModeWrite:
                *outMode = SFM_WRITE;
                return true;
            default:
                return false;
        }
    }
} // namespace

extern "C" {
static Methcla_Error soundfile_close(Methcla_SoundFile*);
static Methcla_Error soundfile_seek(Methcla_SoundFile*, int64_t);
static Methcla_Error soundfile_tell(Methcla_SoundFile*, int64_t*);
static Methcla_Error soundfile_read_float(Methcla_SoundFile*, float*, size_t,
                                          size_t*);
static Methcla_Error soundfile_open(Methcla_SoundFileAPI*, const char*,
                                    Methcla_FileMode, Methcla_SoundFile**,
                                    Methcla_SoundFileInfo*);
} // extern "C"

static Methcla_Error soundfile_close(Methcla_SoundFile* file)
{
    SoundFileHandle::Destructor()(static_cast<SoundFileHandle*>(file->handle));
    return methcla_no_error();
}

static Methcla_Error soundfile_seek(Methcla_SoundFile* file, int64_t numFrames)
{
    SoundFileHandle* handle = static_cast<SoundFileHandle*>(file->handle);
    sf_count_t       n =
        sf_seek(handle->sndfile, static_cast<sf_count_t>(numFrames), SEEK_SET);
    return n >= 0 ? methcla_no_error() : handle->error();
}

static Methcla_Error soundfile_tell(Methcla_SoundFile* file, int64_t* numFrames)
{
    SoundFileHandle* handle = static_cast<SoundFileHandle*>(file->handle);
    sf_count_t       n = sf_seek(handle->sndfile, 0, SEEK_CUR);
    if (n < 0)
        return handle->error();
    *numFrames = static_cast<int64_t>(n);
    return methcla_no_error();
}

static Methcla_Error soundfile_read_float(Methcla_SoundFile* file,
                                          float* buffer, size_t inNumFrames,
                                          size_t* outNumFrames)
{
    SoundFileHandle* handle = static_cast<SoundFileHandle*>(file->handle);

    sf_count_t n = sf_readf_float(handle->sndfile, buffer, inNumFrames);
    if (n < 0)
        return handle->error();
    assert(n <= (sf_count_t)inNumFrames);

    *outNumFrames = n;

    return methcla_no_error();
}

static Methcla_Error soundfile_open(Methcla_SoundFileAPI*, const char* path,
                                    Methcla_FileMode       mode,
                                    Methcla_SoundFile**    outFile,
                                    Methcla_SoundFileInfo* info)
{
    if (path == nullptr)
        return methcla_error_new(kMethcla_ArgumentError);
    if (outFile == nullptr)
        return methcla_error_new(kMethcla_ArgumentError);
    int sfmode = SFM_READ;
    if (!convertMode(mode, &sfmode))
        return methcla_error_new(kMethcla_ArgumentError);

    SoundFileHandle* handle =
        static_cast<SoundFileHandle*>(std::malloc(sizeof(SoundFileHandle)));
    if (handle == nullptr)
        return methcla_error_new(kMethcla_MemoryError);

    auto handleRef = SoundFileHandle::Ref(handle);

    SF_INFO sfinfo;
    handle->sndfile = sf_open(path, sfmode, &sfinfo);
    if (handle->sndfile == nullptr)
        return handle->error();

    Methcla_SoundFile* file = &handle->soundFile;
    memset(file, 0, sizeof(*file));
    file->handle = handle;
    file->close = soundfile_close;
    file->seek = soundfile_seek;
    file->tell = soundfile_tell;
    file->read_float = soundfile_read_float;

    *outFile = file;

    if (info != nullptr)
    {
        info->frames = sfinfo.frames;
        info->channels = sfinfo.channels;
        info->samplerate = sfinfo.samplerate;
    }

    // METHCLA_PRINT_DEBUG("soundfile_open: %s %lld %u %u", path, info->frames,
    // info->channels, info->samplerate);

    handleRef.release();

    return methcla_no_error();
}

static Methcla_SoundFileAPI kSoundFileAPI = {nullptr, nullptr, soundfile_open};

METHCLA_EXPORT Methcla_Library*
               METHCLA_PLUGIN_LOAD(methcla_soundfile_api_libsndfile)(Methcla_Host* host,
                                                          const char*)
{
    methcla_host_register_soundfile_api(host, &kSoundFileAPI);
    return nullptr;
}
