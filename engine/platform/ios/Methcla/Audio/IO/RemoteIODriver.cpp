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

#include "Methcla/Audio/IO/RemoteIODriver.hpp"
#include "Methcla/Exception.hpp"
#include "Methcla/Memory.hpp"

#include <methcla/common.h>

#include <AudioToolbox/AudioToolbox.h>

#include <cassert>
#include <stdexcept>
#include <string>

using namespace Methcla;
using namespace Methcla::Audio::IO;
using Methcla::Audio::sample_t;

#define METHCLA_CASE_SYSTEM_ERROR(status) \
    case status: throw SystemError(#status)

static void throwAudioServicesError(OSStatus status)
{
    switch (status) {
        METHCLA_CASE_SYSTEM_ERROR(kAudioServicesUnsupportedPropertyError);
        METHCLA_CASE_SYSTEM_ERROR(kAudioServicesBadPropertySizeError);
        METHCLA_CASE_SYSTEM_ERROR(kAudioServicesBadSpecifierSizeError);
        METHCLA_CASE_SYSTEM_ERROR(kAudioServicesSystemSoundUnspecifiedError);
        METHCLA_CASE_SYSTEM_ERROR(kAudioServicesSystemSoundClientTimedOutError);
    }
}

static void throwAudioSessionError(OSStatus status)
{
    switch (status) {
        METHCLA_CASE_SYSTEM_ERROR(kAudioSessionNoError);
        METHCLA_CASE_SYSTEM_ERROR(kAudioSessionNotInitialized);
        METHCLA_CASE_SYSTEM_ERROR(kAudioSessionAlreadyInitialized);
        METHCLA_CASE_SYSTEM_ERROR(kAudioSessionInitializationError);
        METHCLA_CASE_SYSTEM_ERROR(kAudioSessionUnsupportedPropertyError);
        METHCLA_CASE_SYSTEM_ERROR(kAudioSessionBadPropertySizeError);
        METHCLA_CASE_SYSTEM_ERROR(kAudioSessionNotActiveError);
        METHCLA_CASE_SYSTEM_ERROR(kAudioServicesNoHardwareError);
        METHCLA_CASE_SYSTEM_ERROR(kAudioSessionNoCategorySet);
        METHCLA_CASE_SYSTEM_ERROR(kAudioSessionIncompatibleCategory);
        METHCLA_CASE_SYSTEM_ERROR(kAudioSessionUnspecifiedError);
    }
}

static void throwAudioUnitError(OSStatus status)
{
    switch (status) {
        METHCLA_CASE_SYSTEM_ERROR(kAudioUnitErr_InvalidProperty);
        METHCLA_CASE_SYSTEM_ERROR(kAudioUnitErr_InvalidParameter);
        METHCLA_CASE_SYSTEM_ERROR(kAudioUnitErr_InvalidElement);
        METHCLA_CASE_SYSTEM_ERROR(kAudioUnitErr_NoConnection);
        METHCLA_CASE_SYSTEM_ERROR(kAudioUnitErr_FailedInitialization);
        METHCLA_CASE_SYSTEM_ERROR(kAudioUnitErr_TooManyFramesToProcess);
        METHCLA_CASE_SYSTEM_ERROR(kAudioUnitErr_InvalidFile);
        METHCLA_CASE_SYSTEM_ERROR(kAudioUnitErr_FormatNotSupported);
        METHCLA_CASE_SYSTEM_ERROR(kAudioUnitErr_Uninitialized);
        METHCLA_CASE_SYSTEM_ERROR(kAudioUnitErr_InvalidScope);
        METHCLA_CASE_SYSTEM_ERROR(kAudioUnitErr_PropertyNotWritable);
        METHCLA_CASE_SYSTEM_ERROR(kAudioUnitErr_CannotDoInCurrentContext);
        METHCLA_CASE_SYSTEM_ERROR(kAudioUnitErr_InvalidPropertyValue);
        METHCLA_CASE_SYSTEM_ERROR(kAudioUnitErr_PropertyNotInUse);
        METHCLA_CASE_SYSTEM_ERROR(kAudioUnitErr_Initialized);
        METHCLA_CASE_SYSTEM_ERROR(kAudioUnitErr_InvalidOfflineRender);
        METHCLA_CASE_SYSTEM_ERROR(kAudioUnitErr_Unauthorized);
    }
}

#define METHCLA_THROW_IF_ERROR(expr, msg) \
    do { \
        OSStatus err__ = expr; \
        if (err__ != noErr) { \
            throwAudioServicesError(err__); \
            throwAudioSessionError(err__); \
            throwAudioUnitError(err__); \
            throw Error(kMethcla_UnspecifiedError); \
        } \
    } while (false)

const AudioUnitElement kOutputBus = 0;
const AudioUnitElement kInputBus = 1;

static void initStreamFormat(AudioStreamBasicDescription& desc, Float64 sampleRate, UInt32 numChannels)
{
    memset(&desc, 0, sizeof(desc));
    desc.mSampleRate = sampleRate;
    desc.mFormatID = kAudioFormatLinearPCM;
    // desc.mFormatFlags = kAudioFormatFlagsNativeFloatPacked | kAudioFormatFlagIsNonInterleaved;
    desc.mFormatFlags = kAudioFormatFlagsCanonical;
    desc.mChannelsPerFrame = numChannels;
    desc.mBitsPerChannel = 8 * sizeof(AudioSampleType);
    desc.mBytesPerFrame = desc.mChannelsPerFrame * sizeof(AudioSampleType);
    desc.mFramesPerPacket = 1;
    desc.mBytesPerPacket = desc.mFramesPerPacket * desc.mBytesPerFrame;
}

RemoteIODriver::RemoteIODriver()
    : m_numInputs(2)
    , m_numOutputs(2)
    , m_inputBuffers(nullptr)
    , m_outputBuffers(nullptr)
{
    // Initialize and configure the audio session
    METHCLA_THROW_IF_ERROR(
        AudioSessionInitialize(NULL, NULL, InterruptionCallback, this)
      , "couldn't initialize audio session");

    UInt32 audioCategory = kAudioSessionCategory_PlayAndRecord;
    METHCLA_THROW_IF_ERROR(
        AudioSessionSetProperty(
            kAudioSessionProperty_AudioCategory
          , sizeof(audioCategory)
          , &audioCategory)
      , "couldn't set audio category");
    //METHCLA_THROW_IF_ERROR(AudioSessionAddPropertyListener(kAudioSessionProperty_AudioRouteChange, propListener, self), "couldn't set property listener");

    // NOTE: This needs to be called *before* trying to determine the
    //       number of input/output channels.
    METHCLA_THROW_IF_ERROR(
        AudioSessionSetActive(true)
      , "couldn't activate audio session");

    Float64 hwSampleRate;
    UInt32 outSize = sizeof(hwSampleRate);
    METHCLA_THROW_IF_ERROR(
        AudioSessionGetProperty(
            kAudioSessionProperty_CurrentHardwareSampleRate
          , &outSize
          , &hwSampleRate)
      , "couldn't get hw sample rate");
    m_sampleRate = hwSampleRate;

    Float32 hwBufferSize;
    outSize = sizeof(hwBufferSize);
    METHCLA_THROW_IF_ERROR(
        AudioSessionGetProperty(
            kAudioSessionProperty_CurrentHardwareIOBufferDuration
          , &outSize
          , &hwBufferSize)
      , "couldn't set i/o buffer duration");
    
    // Check whether input is available
    UInt32 inputAvailable;
    outSize = sizeof(inputAvailable);
    METHCLA_THROW_IF_ERROR(
        AudioSessionGetProperty(
            kAudioSessionProperty_AudioInputAvailable
          , &outSize
          , &inputAvailable)
      , "couldn't determine whether audio input is available");

    // Number of hardware inputs
    if (m_numInputs > 0) {
        UInt32 hwNumInputs = 0;
        if (inputAvailable) {
            outSize = sizeof(hwNumInputs);
            METHCLA_THROW_IF_ERROR(
                AudioSessionGetProperty(
                    kAudioSessionProperty_CurrentHardwareInputNumberChannels
                  , &outSize
                  , &hwNumInputs)
              , "couldn't determine number of hardware input channels");
        }
        m_numInputs = hwNumInputs;
    }

    // Number of hardware outputs
    UInt32 hwNumOutputs;
    outSize = sizeof(m_numOutputs);
    METHCLA_THROW_IF_ERROR(
        AudioSessionGetProperty(
            kAudioSessionProperty_CurrentHardwareOutputNumberChannels
          , &outSize
          , &hwNumOutputs)
     ,  "couldn't determine number of hardware output channels");
    m_numOutputs = hwNumOutputs;

    // Float32 preferredBufferSize = .005;
    // METHCLA_THROW_IF_ERROR(
    //     AudioSessionSetProperty(kAudioSessionProperty_PreferredHardwareIOBufferDuration, sizeof(preferredBufferSize), &preferredBufferSize)
    //   , "couldn't set i/o buffer duration");
    
    // Find remote I/O audio component
    AudioComponentDescription desc;
    desc.componentType = kAudioUnitType_Output;
    desc.componentSubType = kAudioUnitSubType_RemoteIO;
    desc.componentManufacturer = kAudioUnitManufacturer_Apple;
    desc.componentFlags = 0;
    desc.componentFlagsMask = 0;
    
    AudioComponent comp = AudioComponentFindNext(NULL, &desc);
    
    // Instantiate remote I/O unit
    METHCLA_THROW_IF_ERROR(
        AudioComponentInstanceNew(comp, &m_rioUnit)
      , "couldn't open the remote I/O unit");

    // Enable output
    UInt32 enableOutput = 1;
    METHCLA_THROW_IF_ERROR(
        AudioUnitSetProperty(
            m_rioUnit
          , kAudioOutputUnitProperty_EnableIO
          , kAudioUnitScope_Output
          , kOutputBus
          , &enableOutput
          , sizeof(enableOutput))
      , "couldn't enable output on the remote I/O unit");

    // Enable input
    if (m_numInputs > 0) {
        UInt32 enableInput = 1;
        METHCLA_THROW_IF_ERROR(
            AudioUnitSetProperty(
               m_rioUnit
             , kAudioOutputUnitProperty_EnableIO
             , kAudioUnitScope_Input
             , kInputBus
             , &enableInput
             , sizeof(enableInput))
          , "couldn't enable input on the remote I/O unit");
    }

    // This needs to be set before initializing the AudioUnit?
    m_bufferSize = (size_t)(m_sampleRate * hwBufferSize + .5);
    UInt32 maxFPS = m_bufferSize;
    METHCLA_THROW_IF_ERROR(
        AudioUnitSetProperty(
            m_rioUnit
          , kAudioUnitProperty_MaximumFramesPerSlice
          , kAudioUnitScope_Global
          , kInputBus
          , &maxFPS
          , sizeof(maxFPS))
      , "couldn't set AudioUnit buffer size");

    METHCLA_THROW_IF_ERROR(
        AudioUnitSetProperty(
            m_rioUnit
          , kAudioUnitProperty_MaximumFramesPerSlice
          , kAudioUnitScope_Global
          , kOutputBus
          , &maxFPS
          , sizeof(maxFPS))
      , "couldn't set AudioUnit buffer size");

    // UInt32 maxFPS = m_bufferSize;
    // METHCLA_THROW_IF_ERROR(
    //     AudioUnitGetProperty(
    //         m_rioUnit
    //       , kAudioUnitProperty_MaximumFramesPerSlice
    //       , kAudioUnitScope_Global
    //       , kOutputBus
    //       , &maxFPS
    //       , &outSize)
    //   , "couldn't get AudioUnit buffer size");
    // m_bufferSize = maxFPS;

    // Set input format
    if (m_numInputs > 0) {
        AudioStreamBasicDescription inputFormat;
        initStreamFormat(inputFormat, m_sampleRate, m_numInputs);
        METHCLA_THROW_IF_ERROR(
            AudioUnitSetProperty(
                m_rioUnit
              , kAudioUnitProperty_StreamFormat
              , kAudioUnitScope_Output
              , kInputBus
              , &inputFormat
              , sizeof(inputFormat))
          , "couldn't set the remote I/O unit's input client format");
    }

    // Set output format
    AudioStreamBasicDescription outputFormat;
    initStreamFormat(outputFormat, m_sampleRate, m_numOutputs);
    METHCLA_THROW_IF_ERROR(
        AudioUnitSetProperty(
            m_rioUnit
          , kAudioUnitProperty_StreamFormat
          , kAudioUnitScope_Input
          , kOutputBus
          , &outputFormat
          , sizeof(outputFormat))
      , "couldn't set the remote I/O unit's output client format");
        
    // outSize = sizeof(outputFormat);
    // METHCLA_THROW_IF_ERROR(
    //     AudioUnitGetProperty(m_rioUnit, kAudioUnitProperty_StreamFormat, kAudioUnitScope_Input, kOutputBus, &outputFormat, &outSize)
    //   , "couldn't get the remote I/O unit's output client format");
    // outputFormat.Print();

    // Set input callback
    if (m_numInputs > 0) {
        AURenderCallbackStruct inputCallback;
        inputCallback.inputProc = InputCallback;
        inputCallback.inputProcRefCon = this;

        METHCLA_THROW_IF_ERROR(
            AudioUnitSetProperty(
                m_rioUnit
              , kAudioOutputUnitProperty_SetInputCallback
              , kAudioUnitScope_Global
              , kInputBus
              , &inputCallback
              , sizeof(inputCallback))
          , "couldn't set remote i/o input callback");
    }

    // Set output callback
    AURenderCallbackStruct outputCallback;
    outputCallback.inputProc = RenderCallback;
    outputCallback.inputProcRefCon = this;

    METHCLA_THROW_IF_ERROR(
        AudioUnitSetProperty(
            m_rioUnit
          , kAudioUnitProperty_SetRenderCallback
          , kAudioUnitScope_Global
          , kOutputBus
          , &outputCallback
          , sizeof(outputCallback))
      , "couldn't set remote i/o output callback");
    
    // Initialize audio unit
    METHCLA_THROW_IF_ERROR(
        AudioUnitInitialize(m_rioUnit)
      , "couldn't initialize the remote I/O unit");

    // Initialize I/O buffers
    m_inputBuffers = makeBuffers(m_numInputs, m_bufferSize);
    m_outputBuffers = makeBuffers(m_numOutputs, m_bufferSize);
}

RemoteIODriver::~RemoteIODriver()
{
    // Stop audio unit
    stop();

    // Free audio unit
    AudioComponentInstanceDispose(m_rioUnit);

    // Free I/O buffers
    freeBuffers(m_numInputs, m_inputBuffers);
    freeBuffers(m_numOutputs, m_outputBuffers);
}

void RemoteIODriver::start()
{
    METHCLA_THROW_IF_ERROR(
        AudioOutputUnitStart(m_rioUnit)
      , "couldn't start remote i/o unit");
}

void RemoteIODriver::stop()
{
    METHCLA_THROW_IF_ERROR(
        AudioOutputUnitStop(m_rioUnit)
      , "couldn't start remote i/o unit");
}

void RemoteIODriver::InterruptionCallback(void *inClientData, UInt32 inInterruption)
{
    RemoteIODriver* self = static_cast<RemoteIODriver*>(inClientData);

    if (inInterruption == kAudioSessionEndInterruption) {
        // make sure we are again the active session
        METHCLA_THROW_IF_ERROR(AudioSessionSetActive(true), "couldn't set audio session active");
        self->start();
    }

    if (inInterruption == kAudioSessionBeginInterruption) {
        self->stop();
    }
}

OSStatus RemoteIODriver::InputCallback(
    void*                       inRefCon, 
    AudioUnitRenderActionFlags* ioActionFlags, 
    const AudioTimeStamp*       inTimeStamp, 
    UInt32                      inBusNumber, 
    UInt32                      inNumberFrames, 
    AudioBufferList*            /* ioData */)
{
    RemoteIODriver* self = static_cast<RemoteIODriver*>(inRefCon);

    AudioBufferList bufferList;
    memset(&bufferList, 0, sizeof(bufferList));
    bufferList.mNumberBuffers = 1;
    bufferList.mBuffers[0].mData = nullptr;

    // Pull data from input bus
    OSStatus err = AudioUnitRender(
        self->m_rioUnit
      , ioActionFlags
      , inTimeStamp
      , inBusNumber
      , inNumberFrames
      , &bufferList);
    if (err != noErr) return err;

    const UInt32 numInputs = self->m_numInputs;
    sample_t** inputBuffers = self->m_inputBuffers;

    for (UInt32 bufCount = 0; bufCount < bufferList.mNumberBuffers; bufCount++) {
        assert( bufferList.mBuffers[bufCount].mNumberChannels == numInputs );

        const AudioSampleType* pcm = static_cast<AudioSampleType*>(bufferList.mBuffers[bufCount].mData);

        // Deinterleave and convert input
        for (UInt32 curChan = 0; curChan < numInputs; curChan++) {
            for (UInt32 curFrame = 0; curFrame < inNumberFrames; curFrame++) {
                inputBuffers[curChan][curFrame] = pcm[curFrame * numInputs + curChan] / 32768.f;
            }
        }
   }

    return noErr;
}

OSStatus RemoteIODriver::RenderCallback(
    void*                       inRefCon, 
    AudioUnitRenderActionFlags* /* ioActionFlags */,
    const AudioTimeStamp*       /* inTimeStamp */,
    UInt32                      /* inBusNumber */,
    UInt32                      inNumberFrames, 
    AudioBufferList*            ioData)
{
    RemoteIODriver* self = static_cast<RemoteIODriver*>(inRefCon);

    const sample_t* const* inputBuffers = self->m_inputBuffers;
    const UInt32 numOutputs = self->m_numOutputs;
    sample_t* const* outputBuffers = self->m_outputBuffers;

    for (size_t bufCount = 0; bufCount < ioData->mNumberBuffers; bufCount++) {
        AudioBuffer& buf = ioData->mBuffers[bufCount];
        BOOST_ASSERT( buf.mNumberChannels == numOutputs );

        AudioSampleType* pcm = static_cast<AudioSampleType*>(buf.mData);

        // Run DSP graph
        self->process(inNumberFrames, inputBuffers, outputBuffers);

        // Convert and interleave output
        for (UInt32 curChan = 0; curChan < numOutputs; curChan++) {
            for (UInt32 curFrame = 0; curFrame < inNumberFrames; curFrame++) {
#if METHCLA_AUDIO_THROUGH
                if (curChan < self->m_numInputs) {
                    pcm[curFrame * numOutputs + curChan] = inputBuffers[curChan][curFrame] * 32767.f;
                } else {
                    pcm[curFrame * numOutputs + curChan] = 0.f;
                }
#else
                pcm[curFrame * numOutputs + curChan] = outputBuffers[curChan][curFrame] * 32767.f;
#endif
            }
        }
    }

    return noErr;
}

Driver* Methcla::Audio::IO::defaultPlatformDriver()
{
    return new RemoteIODriver();
}
