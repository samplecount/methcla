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

#include <AudioToolbox/AudioToolbox.h>

using namespace Methcla::Audio::IO;

#define METHCLA_THROW_IF_ERROR(expr, msg) \
    do { \
        OSStatus err__ = expr; \
        if (err__ != 0) { \
            BOOST_THROW_EXCEPTION(Methcla::Audio::IO::Exception() \
                    << Methcla::ErrorInfoString(msg) \
                    << OSStatusInfo(err__)); \
        } \
    } while (false);

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

RemoteIODriver::RemoteIODriver() throw (IO::Exception)
    : m_numInputs(2)
    , m_numOutputs(2)
    , m_inputBuffers(0)
    , m_outputBuffers(0)
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
          ,	&outSize
          ,	&inputAvailable)
      , "couldn't determine whether audio input is available");

	// Number of hardware inputs
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
    m_inputBuffers = new sample_t*[m_numInputs];
    m_outputBuffers = new sample_t*[m_numOutputs];    
    for (size_t i = 0; i < m_numInputs; i++) {
        m_inputBuffers[i] =
            Methcla::Memory::allocAlignedOf<sample_t,Methcla::Memory::kSIMDAlignment>(m_bufferSize);
    }
    for (size_t i = 0; i < m_numOutputs; i++) {
        m_outputBuffers[i] =
            Methcla::Memory::allocAlignedOf<sample_t,Methcla::Memory::kSIMDAlignment>(m_bufferSize);
    }
}

RemoteIODriver::~RemoteIODriver()
{
	// Free audio units
	AudioComponentInstanceDispose(m_rioUnit);

    // Free input buffer memory
    for (size_t i=0; i < m_numInputs; i++) {
        Methcla::Memory::free(m_inputBuffers[i]);
    }

    // Free buffer pointer arrays
    delete [] m_inputBuffers;
    delete [] m_outputBuffers;
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
    bufferList.mNumberBuffers = 1;
    bufferList.mBuffers[0].mData = nullptr;

    AudioBufferList* ioData = &bufferList;

	// Pull data from input bus
    OSStatus err = AudioUnitRender(
        self->m_rioUnit
      , ioActionFlags
      , inTimeStamp
      , inBusNumber
      , inNumberFrames
      , ioData);
    if (err != noErr) return err;

	const UInt32 numInputs = self->m_numInputs;
	sample_t** inputBuffers = self->m_inputBuffers;

	for (size_t bufCount = 0; bufCount < ioData->mNumberBuffers; bufCount++) {
		AudioBuffer& buf = ioData->mBuffers[bufCount];
		BOOST_ASSERT( buf.mNumberChannels == numInputs );

		AudioSampleType* pcm = static_cast<AudioSampleType*>(buf.mData);

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
    AudioUnitRenderActionFlags* ioActionFlags, 
    const AudioTimeStamp*       inTimeStamp, 
    UInt32                      inBusNumber, 
    UInt32                      inNumberFrames, 
    AudioBufferList*            ioData)
{
    RemoteIODriver* self = static_cast<RemoteIODriver*>(inRefCon);

	sample_t** inputBuffers = self->m_inputBuffers;
	const UInt32 numOutputs = self->m_numOutputs;
	sample_t** outputBuffers = self->m_outputBuffers;

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
