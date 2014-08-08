// Copyright 2014 Samplecount S.L.
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

#include <methcla/plugins/soundfile_api_mpg123.h>

#include <mpg123.h>

#include <algorithm>
#include <memory>
#include <cstdio>
#include <cstdlib>
#include <cstring>

struct SoundFileHandle
{
    mpg123_handle*    handle;
    size_t            frameSize;
    Methcla_SoundFile soundFile;

    struct Destructor
    {
        void operator()(SoundFileHandle* handle)
        {
            if (handle != nullptr)
            {
                if (handle->handle != nullptr)
                {
                    mpg123_close(handle->handle);
                    mpg123_delete(handle->handle);
                }
                free(handle);
            }
        }
    };

    typedef std::unique_ptr<SoundFileHandle,Destructor> Ref;

    Methcla_Error error(Methcla_ErrorCode code=kMethcla_UnspecifiedError)
    {
        return methcla_error_new_with_message(
            code,
            mpg123_strerror(handle)
        );
    }
};

extern "C"
{
    static Methcla_Error soundfile_close(const Methcla_SoundFile*);
    static Methcla_Error soundfile_seek(const Methcla_SoundFile*, int64_t);
    static Methcla_Error soundfile_tell(const Methcla_SoundFile*, int64_t*);
    static Methcla_Error soundfile_read_float(const Methcla_SoundFile*, float*, size_t, size_t*);
    static Methcla_Error soundfile_open(const Methcla_SoundFileAPI*, const char*, Methcla_FileMode, Methcla_SoundFile**, Methcla_SoundFileInfo*);
} // extern "C"

static Methcla_Error soundfile_close(const Methcla_SoundFile* file)
{
    SoundFileHandle::Destructor()(static_cast<SoundFileHandle*>(file->handle));
    return methcla_no_error();
}

static Methcla_Error soundfile_seek(const Methcla_SoundFile* file, int64_t numFrames)
{
    SoundFileHandle* handle = static_cast<SoundFileHandle*>(file->handle);
    off_t n = mpg123_seek(handle->handle, static_cast<off_t>(std::max<int64_t>(0, numFrames)), SEEK_SET);
    return n >= 0 ? methcla_no_error() : handle->error();
}

static Methcla_Error soundfile_tell(const Methcla_SoundFile* file, int64_t* numFrames)
{
    SoundFileHandle* handle = static_cast<SoundFileHandle*>(file->handle);
    off_t n = mpg123_tell(handle->handle);
    if (n < 0) return handle->error();
    *numFrames = static_cast<int64_t>(n);
    return methcla_no_error();
}

static Methcla_Error soundfile_read_float(const Methcla_SoundFile* file, float* buffer, size_t inNumFrames, size_t* outNumFrames)
{
    SoundFileHandle* handle = static_cast<SoundFileHandle*>(file->handle);

    size_t numBytes = 0;

    int err = mpg123_read(
        handle->handle,
        reinterpret_cast<unsigned char*>(buffer),
        inNumFrames * handle->frameSize,
        &numBytes
    );

    if (err == MPG123_DONE)
    {
        *outNumFrames = 0;
        return methcla_no_error();
    }
    else if (err != MPG123_OK)
    {
        return handle->error();
    }

    *outNumFrames = numBytes / handle->frameSize;

    return methcla_no_error();
}

static Methcla_Error soundfile_open(const Methcla_SoundFileAPI*, const char* path, Methcla_FileMode mode, Methcla_SoundFile** outFile, Methcla_SoundFileInfo* info)
{
    if (path == nullptr)
        return methcla_error_new(kMethcla_ArgumentError);
    if (outFile == nullptr)
        return methcla_error_new(kMethcla_ArgumentError);
    if (mode != kMethcla_FileModeRead)
        return methcla_error_new(kMethcla_ArgumentError);

    SoundFileHandle* handle = static_cast<SoundFileHandle*>(std::malloc(sizeof(SoundFileHandle)));
    if (handle == nullptr)
        return methcla_error_new(kMethcla_MemoryError);

    auto handleRef = SoundFileHandle::Ref(handle);

    int err;
    handle->handle = mpg123_new(nullptr, &err);
    if (handle->handle == nullptr)
        return methcla_error_new(kMethcla_MemoryError);

    if (mpg123_param(handle->handle, MPG123_ADD_FLAGS, MPG123_FORCE_FLOAT, 0.) != MPG123_OK)
        return handle->error();

    if (mpg123_open(handle->handle, path) != MPG123_OK)
        return handle->error();

    // Check number of frames in file here because mpg123_open doesn't return an error for an invalid mp3 file
    const off_t frames = mpg123_length(handle->handle);
    if (frames < 0)
        return methcla_error_new(kMethcla_UnsupportedFileTypeError);

    long rate;
    int channels, encoding;

    if (mpg123_getformat(handle->handle, &rate, &channels, &encoding) != MPG123_OK)
        return handle->error();

    if (encoding != MPG123_ENC_FLOAT_32)
        return methcla_error_new(kMethcla_UnsupportedDataFormatError);

    // Ensure that output format doesn't change (it could, if we allowed it).
    mpg123_format_none(handle->handle);
    mpg123_format(handle->handle, rate, channels, encoding);

    handle->frameSize = channels * sizeof(float);

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
        info->frames = frames;
        info->channels = channels;
        info->samplerate = rate;
    }

    // METHCLA_PRINT_DEBUG("soundfile_open: %s %lld %u %u", path, info->frames, info->channels, info->samplerate);

    handleRef.release();

    return methcla_no_error();
}

static void methcla_mpg123_library_destroy(const Methcla_Library*)
{
    mpg123_exit();
}

static const Methcla_SoundFileAPI kSoundFileAPI_mpg123 = {
    nullptr,
    soundfile_open
};

static const Methcla_Library kLibrary_mpg123 =
{
    nullptr,
    methcla_mpg123_library_destroy
};

METHCLA_EXPORT const Methcla_Library* methcla_soundfile_api_mpg123(const Methcla_Host* host, const char*)
{
    if (mpg123_init() != MPG123_OK)
        return nullptr;
    methcla_host_register_soundfile_api(host, &kSoundFileAPI_mpg123);
    return &kLibrary_mpg123;
}
