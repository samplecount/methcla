#include <Mescaline/Audio/IO/RemoteIODriver.h>
#include <AudioToolbox/AudioToolbox.h>
#include "CAXException.h"

using namespace Mescaline::Audio::IO;

RemoteIODriver::RemoteIODriver(Client* client)
    : m_client(client)
    , m_numInputs(2)
    , m_numOutputs(2)
    , m_inputBuffers(0)
    , m_outputBuffers(0)
    , m_CAInputBuffers(0)
{
    // Initialize and configure the audio session
    XThrowIfError(
        AudioSessionInitialize(NULL, NULL, InterruptionCallback, this)
      , "couldn't initialize audio session");
    
    XThrowIfError(
        AudioSessionSetActive(true)
      , "couldn't set audio session active\n");
    
    UInt32 audioCategory = kAudioSessionCategory_PlayAndRecord;
    XThrowIfError(
        AudioSessionSetProperty(kAudioSessionProperty_AudioCategory, sizeof(audioCategory), &audioCategory)
      , "couldn't set audio category");
    //XThrowIfError(AudioSessionAddPropertyListener(kAudioSessionProperty_AudioRouteChange, propListener, self), "couldn't set property listener");
    
    Float64 hwSampleRate;
    UInt32 outSize = sizeof(hwSampleRate);
    XThrowIfError(
        AudioSessionGetProperty(kAudioSessionProperty_CurrentHardwareSampleRate, &outSize, &hwSampleRate)
      , "couldn't get hw sample rate");
    m_sampleRate = hwSampleRate;

    Float32 hwBufferSize;
    outSize = sizeof(hwBufferSize);
	XThrowIfError(
	    AudioSessionGetProperty(kAudioSessionProperty_CurrentHardwareIOBufferDuration, &outSize, &hwBufferSize)
      , "couldn't set i/o buffer duration");
    
    outSize = sizeof(m_numInputs);
	XThrowIfError(
	    AudioSessionGetProperty(kAudioSessionProperty_CurrentHardwareInputNumberChannels, &outSize, &m_numInputs)
      , "couldn't get hardware input channels");
    
    outSize = sizeof(m_numOutputs);
	XThrowIfError(
	    AudioSessionGetProperty(kAudioSessionProperty_CurrentHardwareOutputNumberChannels, &outSize, &m_numOutputs)
     ,  "couldn't get hardware output channels");

    // Float32 preferredBufferSize = .005;
    // XThrowIfError(
    //     AudioSessionSetProperty(kAudioSessionProperty_PreferredHardwareIOBufferDuration, sizeof(preferredBufferSize), &preferredBufferSize)
    //   , "couldn't set i/o buffer duration");
    
    // Open the output unit
    AudioComponentDescription desc;
    desc.componentType = kAudioUnitType_Output;
    desc.componentSubType = kAudioUnitSubType_RemoteIO;
    desc.componentManufacturer = kAudioUnitManufacturer_Apple;
    desc.componentFlags = 0;
    desc.componentFlagsMask = 0;
    
    AudioComponent comp = AudioComponentFindNext(NULL, &desc);
    
    XThrowIfError(
        AudioComponentInstanceNew(comp, &m_rioUnit)
      , "couldn't open the remote I/O unit");
    
    UInt32 enableOutput = 1;
    XThrowIfError(
        AudioUnitSetProperty(m_rioUnit, kAudioOutputUnitProperty_EnableIO, kAudioUnitScope_Output, 0, &enableOutput, sizeof(enableOutput))
      , "couldn't enable output on the remote I/O unit");
    UInt32 enableInput = 1;
    XThrowIfError(
        AudioUnitSetProperty(m_rioUnit, kAudioOutputUnitProperty_EnableIO, kAudioUnitScope_Input, 1, &enableInput, sizeof(enableInput))
      , "couldn't enable input on the remote I/O unit");
    
    // This needs to be set before initializing the AudioUnit?
    m_bufferSize = (size_t)(m_sampleRate * hwBufferSize + .5);
    UInt32 maxFPS = m_bufferSize;
    XThrowIfError(
        AudioUnitSetProperty(m_rioUnit, kAudioUnitProperty_MaximumFramesPerSlice, kAudioUnitScope_Global, 1, &maxFPS, sizeof(maxFPS))
      , "couldn't set AudioUnit buffer size");
    
    XThrowIfError(
        AudioUnitSetProperty(m_rioUnit, kAudioUnitProperty_MaximumFramesPerSlice, kAudioUnitScope_Global, 0, &maxFPS, sizeof(maxFPS))
      , "couldn't set AudioUnit buffer size");

    // XThrowIfError(
    //     AudioUnitGetProperty(m_rioUnit, kAudioUnitProperty_MaximumFramesPerSlice, kAudioUnitScope_Global, 0, &m_bufferSize, &outSize)
    //   , "couldn't get AudioUnit buffer size");
    
    XThrowIfError(
        AudioUnitInitialize(m_rioUnit)
      , "couldn't initialize the remote I/O unit");

    // Set our required format - Canonical AU format: LPCM non-interleaved 8.24 fixed point
    CAStreamBasicDescription outFormat;
    outFormat.SetAUCanonical(m_numOutputs, false);
    outFormat.mSampleRate = m_sampleRate;
    XThrowIfError(
        AudioUnitSetProperty(m_rioUnit, kAudioUnitProperty_StreamFormat, kAudioUnitScope_Input, 0, &outFormat, sizeof(outFormat))
      , "couldn't set the remote I/O unit's output client format");
    XThrowIfError(AudioUnitSetProperty(m_rioUnit, kAudioUnitProperty_StreamFormat, kAudioUnitScope_Output, 1, &outFormat, sizeof(outFormat))
      , "couldn't set the remote I/O unit's input client format");
        
    outSize = sizeof(outFormat);
    XThrowIfError(
        AudioUnitGetProperty(m_rioUnit, kAudioUnitProperty_StreamFormat, kAudioUnitScope_Output, 1, &outFormat, &outSize)
      , "couldn't get the remote I/O unit's output client format");
    outFormat.Print();

    // Set render callback
    AURenderCallbackStruct inputProc;
    inputProc.inputProc = RenderCallback;
    inputProc.inputProcRefCon = this;

    XThrowIfError(
        AudioUnitSetProperty(m_rioUnit, kAudioUnitProperty_SetRenderCallback, kAudioUnitScope_Input, 0, &inputProc, sizeof(inputProc))
      , "couldn't set remote i/o render callback");
    
    // Initialize I/O buffers
    m_inputBuffers = new float*[m_numInputs];
    m_outputBuffers = new float*[m_numInputs];
    m_CAInputBuffers = new AudioBufferList[m_numInputs];
    m_CAInputBuffers->mNumberBuffers = m_numInputs;
    
    for (size_t i = 0; i < m_numInputs; i++) {
        m_inputBuffers[i] = new float[m_bufferSize];
        m_CAInputBuffers->mBuffers[i].mNumberChannels = 1;
        m_CAInputBuffers->mBuffers[i].mDataByteSize = m_bufferSize * sizeof(AudioUnitSampleType);
        m_CAInputBuffers->mBuffers[i].mData = m_inputBuffers[i];
    }
    
    // Initialize client
    client->configure(*this);
}

RemoteIODriver::~RemoteIODriver()
{
    for (size_t i=0; i < m_numInputs; i++) {
        m_inputBuffers[i];
    }
    delete [] m_inputBuffers;
    delete [] m_outputBuffers;
    delete [] m_CAInputBuffers;
}

void RemoteIODriver::start()
{
    XThrowIfError(AudioOutputUnitStart(m_rioUnit), "couldn't start remote i/o unit");
}

void RemoteIODriver::stop()
{
    XThrowIfError(AudioOutputUnitStop(m_rioUnit), "couldn't start remote i/o unit");
}

void RemoteIODriver::InterruptionCallback(void *inClientData, UInt32 inInterruption)
{
    RemoteIODriver* self = static_cast<RemoteIODriver*>(inClientData);

	if (inInterruption == kAudioSessionEndInterruption) {
		// make sure we are again the active session
		XThrowIfError(AudioSessionSetActive(true), "couldn't set audio session active");
        self->start();
	}

	if (inInterruption == kAudioSessionBeginInterruption) {
        self->stop();
    }
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
    
    // Gather input
    OSStatus err = AudioUnitRender(self->m_rioUnit, ioActionFlags, inTimeStamp, 1, inNumberFrames, self->m_CAInputBuffers);
    if (err) return err;

    // Set up output buffers
    for (size_t i = 0; i < self->numOutputs(); i++) {
        self->m_outputBuffers[i] = (float*)ioData->mBuffers[i].mData;
    }

    // Run DSP graph
    self->m_client->process(inNumberFrames, self->m_inputBuffers, self->m_outputBuffers);

    return noErr;
}
