// Copyright 2012-2013 Samplecount S.L.
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

#include <methcla/plugins/soundfile_api_extaudiofile.h>

#include <iostream>
#include <memory>
#include <sstream>

#include <AudioToolbox/AudioToolbox.h>

struct SoundFileHandle
{
    ExtAudioFileRef         file;
    size_t                  numChannels;
    double                  sampleRate;
    Methcla_FileMode        mode;
    Methcla_SoundFileFormat format;
    Methcla_SoundFile       soundFile;
};

extern "C" {
static Methcla_Error soundfile_close(const Methcla_SoundFile*);
static Methcla_Error soundfile_seek(const Methcla_SoundFile*, int64_t);
static Methcla_Error soundfile_tell(const Methcla_SoundFile*, int64_t*);
static Methcla_Error soundfile_read_float(const Methcla_SoundFile*, float*,
                                          size_t, size_t*);
static Methcla_Error soundfile_write_float(const Methcla_SoundFile*,
                                           const float*, size_t, size_t*);
static Methcla_Error soundfile_open(const Methcla_SoundFileAPI*, const char*,
                                    Methcla_FileMode, Methcla_SoundFile**,
                                    Methcla_SoundFileInfo*);
}

Methcla_Error soundfile_close(const Methcla_SoundFile* file)
{
    //    std::cout << "extAudioFile_close " << file << " " << file->handle <<
    //    std::endl;
    SoundFileHandle* handle = (SoundFileHandle*)file->handle;
    if (handle->file != nullptr)
    {
        ExtAudioFileDispose(handle->file);
        handle->file = nullptr;
    }
    free(handle);
    return methcla_no_error();
}

Methcla_Error soundfile_seek(const Methcla_SoundFile* file, int64_t numFrames)
{
    ExtAudioFileRef extFile = ((SoundFileHandle*)file->handle)->file;
    if (extFile == nullptr)
        return methcla_error_new(kMethcla_ArgumentError);

    OSStatus err = ExtAudioFileSeek(extFile, numFrames);
    if (err != noErr)
        return methcla_error_new(kMethcla_UnspecifiedError);

    return methcla_no_error();
}

Methcla_Error soundfile_tell(const Methcla_SoundFile* file, int64_t* numFrames)
{
    ExtAudioFileRef extFile = ((SoundFileHandle*)file->handle)->file;
    if (extFile == nullptr)
        return methcla_error_new(kMethcla_ArgumentError);

    SInt64   outNumFrames;
    OSStatus err = ExtAudioFileTell(extFile, &outNumFrames);
    if (err != noErr)
        return methcla_error_new(kMethcla_UnspecifiedError);

    *numFrames = outNumFrames;

    return methcla_no_error();
}

AudioStreamBasicDescription floatClientFormat(double sampleRate,
                                              size_t numChannels)
{
    AudioStreamBasicDescription clientFormat;
    clientFormat.mSampleRate = sampleRate;
    clientFormat.mFormatID = kAudioFormatLinearPCM;
    clientFormat.mChannelsPerFrame = numChannels;
    clientFormat.mBitsPerChannel = sizeof(float) * 8;
    clientFormat.mBytesPerPacket = clientFormat.mBytesPerFrame =
        sizeof(float) * clientFormat.mChannelsPerFrame;
    clientFormat.mFramesPerPacket = 1;
    clientFormat.mFormatFlags = kAudioFormatFlagsNativeFloatPacked;
    return clientFormat;
}

Methcla_Error soundfile_read_float(const Methcla_SoundFile* file, float* buffer,
                                   size_t inNumFrames, size_t* outNumFrames)
{
    if (file == nullptr || buffer == nullptr || outNumFrames == nullptr)
        return methcla_error_new(kMethcla_ArgumentError);

    SoundFileHandle* handle = (SoundFileHandle*)file->handle;
    if (handle->mode != kMethcla_FileModeRead)
        return methcla_error_new(kMethcla_ArgumentError);

    ExtAudioFileRef extFile = handle->file;
    if (extFile == nullptr)
        return methcla_error_new(kMethcla_ArgumentError);

    UInt32 numFrames = inNumFrames;

    AudioBufferList bufferList;
    bufferList.mNumberBuffers = 1;
    bufferList.mBuffers[0].mNumberChannels = handle->numChannels;
    bufferList.mBuffers[0].mDataByteSize =
        handle->numChannels * inNumFrames * sizeof(float);
    bufferList.mBuffers[0].mData = buffer;

    OSStatus err = ExtAudioFileRead(extFile, &numFrames, &bufferList);
    if (err != noErr)
        return methcla_error_new(kMethcla_UnspecifiedError);

    *outNumFrames = numFrames;

    return methcla_no_error();
}

static void convert16(void* dst, const float* src, size_t n)
{
    for (size_t i = 0; i < n; i++)
    {
        static_cast<int16_t*>(dst)[i] = src[i] * 32767.f;
    }
}

static void convert32(void* dst, const float* src, size_t n)
{
    for (size_t i = 0; i < n; i++)
    {
        static_cast<int32_t*>(dst)[i] = src[i] * 2147483648.f;
    }
}

Methcla_Error soundfile_write_float(const Methcla_SoundFile* file,
                                    const float* buffer, size_t inNumFrames,
                                    size_t* outNumFrames)
{
    if (file == nullptr || buffer == nullptr || outNumFrames == nullptr)
        return methcla_error_new(kMethcla_ArgumentError);

    SoundFileHandle* handle = (SoundFileHandle*)file->handle;
    if (handle->mode != kMethcla_FileModeWrite)
        return methcla_error_new(kMethcla_ArgumentError);

    ExtAudioFileRef extFile = handle->file;
    if (extFile == nullptr)
        return methcla_error_new(kMethcla_ArgumentError);

    if (handle->format == kMethcla_SoundFileFormatFloat)
    {
        AudioBufferList bufferList;
        bufferList.mNumberBuffers = 1;
        bufferList.mBuffers[0].mNumberChannels = handle->numChannels;
        bufferList.mBuffers[0].mDataByteSize =
            handle->numChannels * inNumFrames * sizeof(float);
        bufferList.mBuffers[0].mData = const_cast<float*>(buffer);

        OSStatus err = ExtAudioFileWrite(extFile, inNumFrames, &bufferList);
        if (err != noErr)
            return methcla_error_new(kMethcla_UnspecifiedError);
    }
    else
    {
        size_t bytesPerSample;
        void (*convert)(void*, const float*, size_t) = nullptr;

        switch (handle->format)
        {
            case kMethcla_SoundFileFormatPCM16:
                bytesPerSample = 2;
                convert = convert16;
                break;
            // case kMethcla_SoundFileFormatPCM24:
            //     bytesPerSample = 3;
            //     convert = convert24;
            //     break;
            case kMethcla_SoundFileFormatPCM32:
                bytesPerSample = 4;
                convert = convert32;
                break;
            default:
                return methcla_error_new(kMethcla_UnsupportedDataFormatError);
        }

        AudioBufferList bufferList;
        bufferList.mNumberBuffers = 1;
        bufferList.mBuffers[0].mNumberChannels = handle->numChannels;
        bufferList.mBuffers[0].mDataByteSize =
            handle->numChannels * inNumFrames * bytesPerSample;
        bufferList.mBuffers[0].mData =
            malloc(bufferList.mBuffers[0].mDataByteSize);

        if (bufferList.mBuffers[0].mData == nullptr)
            return methcla_error_new(kMethcla_MemoryError);

        convert(bufferList.mBuffers[0].mData, buffer,
                handle->numChannels * inNumFrames);

        OSStatus err = ExtAudioFileWrite(extFile, inNumFrames, &bufferList);

        free(bufferList.mBuffers[0].mData);

        if (err != noErr)
            return methcla_error_new(kMethcla_UnspecifiedError);
    }

    *outNumFrames = inNumFrames;

    return methcla_no_error();
}

// Helper class for releasing a CFTypeRef when it goes out of scope
template <class T> class CFRef
{
public:
    CFRef(T ref)
    : m_ref(ref)
    {}
    ~CFRef() { CFRelease(m_ref); }

    CFRef(const CFRef<T>& other) = delete;
    CFRef<T>& operator=(const CFRef<T>& other) = delete;

    operator T() { return m_ref; }

private:
    T m_ref;
};

Methcla_Error soundfile_open(const Methcla_SoundFileAPI*, const char* path,
                             Methcla_FileMode mode, Methcla_SoundFile** outFile,
                             Methcla_SoundFileInfo* info)
{
    if (path == nullptr)
        return methcla_error_new(kMethcla_ArgumentError);
    if (outFile == nullptr)
        return methcla_error_new(kMethcla_ArgumentError);
    if (info == nullptr)
        return methcla_error_new(kMethcla_ArgumentError);

    CFRef<CFURLRef> pathURL(CFURLCreateFromFileSystemRepresentation(
        kCFAllocatorDefault, (UInt8*)path, strlen(path), false));
    ExtAudioFileRef extFile = nullptr;

    if (mode == kMethcla_FileModeRead)
    {
        // Open the source file
        OSStatus err = ExtAudioFileOpenURL(pathURL, &extFile);
        if (err != noErr)
        {
            std::stringstream s;
            s << "OSStatus " << err;
            return methcla_error_new_with_message(kMethcla_UnspecifiedError,
                                                  s.str().c_str());
        }

        AudioStreamBasicDescription srcFormat;
        UInt32                      size = sizeof(srcFormat);
        err = ExtAudioFileGetProperty(
            extFile, kExtAudioFileProperty_FileDataFormat, &size, &srcFormat);
        if (err != noErr)
        {
            ExtAudioFileDispose(extFile);
            return methcla_error_new(kMethcla_UnspecifiedError);
        }

        SInt64 numFrames;
        size = sizeof(numFrames);
        err = ExtAudioFileGetProperty(
            extFile, kExtAudioFileProperty_FileLengthFrames, &size, &numFrames);
        if (err != noErr)
        {
            ExtAudioFileDispose(extFile);
            return methcla_error_new(kMethcla_UnspecifiedError);
        }

        info->frames = numFrames;
        info->channels = srcFormat.mChannelsPerFrame;
        info->samplerate = srcFormat.mSampleRate;

        AudioStreamBasicDescription clientFormat(
            floatClientFormat(info->samplerate, info->channels));
        err = ExtAudioFileSetProperty(extFile,
                                      kExtAudioFileProperty_ClientDataFormat,
                                      sizeof(clientFormat), &clientFormat);
        if (err != noErr)
            return methcla_error_new(kMethcla_UnsupportedDataFormatError);
    }
    else if (mode == kMethcla_FileModeWrite)
    {
        AudioFileTypeID fileType;

        switch (info->file_type)
        {
            case kMethcla_SoundFileTypeUnknown:
                return methcla_error_new(kMethcla_ArgumentError);
            case kMethcla_SoundFileTypeAIFF:
                fileType = kAudioFileAIFFType;
                break;
            case kMethcla_SoundFileTypeWAV:
                fileType = kAudioFileWAVEType;
                break;
            default:
                return methcla_error_new(kMethcla_UnsupportedFileTypeError);
        }

        AudioStreamBasicDescription streamDesc;
        memset(&streamDesc, 0, sizeof(streamDesc));

        if (info->channels > std::numeric_limits<uint16_t>::max())
            return methcla_error_new(kMethcla_ArgumentError);
        const uint16_t numChannels = info->channels;

        streamDesc.mSampleRate = info->samplerate;
        streamDesc.mFormatFlags =
            kAudioFormatFlagsNativeEndian | kAudioFormatFlagIsPacked;
        streamDesc.mChannelsPerFrame = numChannels;
        streamDesc.mFramesPerPacket = 1;

        switch (info->file_format)
        {
            case kMethcla_SoundFileFormatUnknown:
                return methcla_error_new(kMethcla_ArgumentError);
            case kMethcla_SoundFileFormatPCM16:
                streamDesc.mFormatID = kAudioFormatLinearPCM;
                streamDesc.mFormatFlags |= kAudioFormatFlagIsSignedInteger;
                streamDesc.mBitsPerChannel = 16;
                streamDesc.mBytesPerFrame = streamDesc.mChannelsPerFrame * 2;
                break;
            // case kMethcla_SoundFileFormatPCM24:
            //     streamDesc.mFormatID = kAudioFormatLinearPCM;
            //     streamDesc.mFormatFlags |= kAudioFormatFlagIsSignedInteger;
            //     streamDesc.mBitsPerChannel = 24;
            //     streamDesc.mBytesPerFrame = streamDesc.mChannelsPerFrame * 3;
            //     break;
            case kMethcla_SoundFileFormatPCM32:
                streamDesc.mFormatID = kAudioFormatLinearPCM;
                streamDesc.mFormatFlags |= kAudioFormatFlagIsSignedInteger;
                streamDesc.mBitsPerChannel = 32;
                streamDesc.mBytesPerFrame = streamDesc.mChannelsPerFrame * 4;
                break;
            case kMethcla_SoundFileFormatFloat:
                streamDesc.mFormatID = kAudioFormatLinearPCM;
                streamDesc.mFormatFlags |= kAudioFormatFlagIsFloat;
                streamDesc.mBitsPerChannel = 32;
                streamDesc.mBytesPerFrame = streamDesc.mChannelsPerFrame * 4;
                break;
            default:
                return methcla_error_new(kMethcla_UnsupportedDataFormatError);
        }

        streamDesc.mBytesPerPacket =
            streamDesc.mFramesPerPacket * streamDesc.mBytesPerFrame;

        AudioChannelLayout channelLayout;
        memset(&channelLayout, 0, sizeof(channelLayout));
        channelLayout.mChannelLayoutTag =
            kAudioChannelLayoutTag_DiscreteInOrder | numChannels;

        OSStatus status = ExtAudioFileCreateWithURL(
            pathURL, fileType, &streamDesc, &channelLayout,
            kAudioFileFlags_EraseFile, &extFile);
        if (status != noErr)
            return methcla_error_new(kMethcla_SystemError);
    }
    else
    {
        return methcla_error_new(kMethcla_ArgumentError);
    }

    assert(extFile != nullptr);

    SoundFileHandle* handle = (SoundFileHandle*)malloc(sizeof(SoundFileHandle));
    if (handle == nullptr)
    {
        ExtAudioFileDispose(extFile);
        return methcla_error_new(kMethcla_MemoryError);
    }

    handle->file = extFile;
    handle->sampleRate = info->samplerate;
    handle->numChannels = info->channels;
    handle->mode = mode;
    handle->format = info->file_format;

    Methcla_SoundFile* file = &handle->soundFile;
    memset(file, 0, sizeof(*file));
    file->handle = handle;
    file->close = soundfile_close;
    file->seek = soundfile_seek;
    file->tell = soundfile_tell;
    file->read_float = soundfile_read_float;
    file->write_float = soundfile_write_float;

    *outFile = file;

    return methcla_no_error();
}

static const Methcla_SoundFileAPI kSoundFileAPI = {nullptr, nullptr,
                                                   soundfile_open};

METHCLA_EXPORT Methcla_Library*
               methcla_soundfile_api_extaudiofile(Methcla_Host* host, const char*)
{
    methcla_host_register_soundfile_api(host, &kSoundFileAPI);
    return nullptr;
}
