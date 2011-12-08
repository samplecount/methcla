#include <Mescaline/Audio/IO/RemoteIODriver.hpp>
#include <Mescaline/Exception.hpp>

#include <AudioToolbox/AudioToolbox.h>

using namespace Mescaline::Audio::IO;

typedef boost::error_info<struct tag_OSStatus,OSStatus> OSStatusInfo;

#define MESCALINE_THROW_IF_ERROR(expr, msg) \
    do { \
        OSStatus err__ = expr; \
        if (err__ != 0) { \
            BOOST_THROW_EXCEPTION(Mescaline::Audio::IO::Exception() \
                    << Mescaline::ErrorInfoString(msg) \
                    << OSStatusInfo(err__)); \
        } \
    } while (false);

RemoteIODriver::RemoteIODriver(Client* client) throw (IO::Exception)
    : m_client(client)
    , m_numInputs(2)
    , m_numOutputs(2)
    , m_inputBuffers(0)
    , m_outputBuffers(0)
    , m_CAInputBuffers(0)
{
    // Initialize and configure the audio session
    MESCALINE_THROW_IF_ERROR(
        AudioSessionInitialize(NULL, NULL, InterruptionCallback, this)
      , "couldn't initialize audio session");
    
    MESCALINE_THROW_IF_ERROR(
        AudioSessionSetActive(true)
      , "couldn't set audio session active\n");
    
    UInt32 audioCategory = kAudioSessionCategory_PlayAndRecord;
    MESCALINE_THROW_IF_ERROR(
        AudioSessionSetProperty(kAudioSessionProperty_AudioCategory, sizeof(audioCategory), &audioCategory)
      , "couldn't set audio category");
    //MESCALINE_THROW_IF_ERROR(AudioSessionAddPropertyListener(kAudioSessionProperty_AudioRouteChange, propListener, self), "couldn't set property listener");
    
    Float64 hwSampleRate;
    UInt32 outSize = sizeof(hwSampleRate);
    MESCALINE_THROW_IF_ERROR(
        AudioSessionGetProperty(kAudioSessionProperty_CurrentHardwareSampleRate, &outSize, &hwSampleRate)
      , "couldn't get hw sample rate");
    m_sampleRate = hwSampleRate;

    Float32 hwBufferSize;
    outSize = sizeof(hwBufferSize);
	MESCALINE_THROW_IF_ERROR(
	    AudioSessionGetProperty(kAudioSessionProperty_CurrentHardwareIOBufferDuration, &outSize, &hwBufferSize)
      , "couldn't set i/o buffer duration");
    
    outSize = sizeof(m_numInputs);
	MESCALINE_THROW_IF_ERROR(
	    AudioSessionGetProperty(kAudioSessionProperty_CurrentHardwareInputNumberChannels, &outSize, &m_numInputs)
      , "couldn't get hardware input channels");
    
    outSize = sizeof(m_numOutputs);
	MESCALINE_THROW_IF_ERROR(
	    AudioSessionGetProperty(kAudioSessionProperty_CurrentHardwareOutputNumberChannels, &outSize, &m_numOutputs)
     ,  "couldn't get hardware output channels");

    // Float32 preferredBufferSize = .005;
    // MESCALINE_THROW_IF_ERROR(
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
    
    MESCALINE_THROW_IF_ERROR(
        AudioComponentInstanceNew(comp, &m_rioUnit)
      , "couldn't open the remote I/O unit");
    
    UInt32 enableOutput = 1;
    MESCALINE_THROW_IF_ERROR(
        AudioUnitSetProperty(m_rioUnit, kAudioOutputUnitProperty_EnableIO, kAudioUnitScope_Output, 0, &enableOutput, sizeof(enableOutput))
      , "couldn't enable output on the remote I/O unit");
    UInt32 enableInput = 1;
    MESCALINE_THROW_IF_ERROR(
        AudioUnitSetProperty(m_rioUnit, kAudioOutputUnitProperty_EnableIO, kAudioUnitScope_Input, 1, &enableInput, sizeof(enableInput))
      , "couldn't enable input on the remote I/O unit");
    
    // This needs to be set before initializing the AudioUnit?
    m_bufferSize = (size_t)(m_sampleRate * hwBufferSize + .5);
    UInt32 maxFPS = m_bufferSize;
    MESCALINE_THROW_IF_ERROR(
        AudioUnitSetProperty(m_rioUnit, kAudioUnitProperty_MaximumFramesPerSlice, kAudioUnitScope_Global, 1, &maxFPS, sizeof(maxFPS))
      , "couldn't set AudioUnit buffer size");
    
    MESCALINE_THROW_IF_ERROR(
        AudioUnitSetProperty(m_rioUnit, kAudioUnitProperty_MaximumFramesPerSlice, kAudioUnitScope_Global, 0, &maxFPS, sizeof(maxFPS))
      , "couldn't set AudioUnit buffer size");

    // MESCALINE_THROW_IF_ERROR(
    //     AudioUnitGetProperty(m_rioUnit, kAudioUnitProperty_MaximumFramesPerSlice, kAudioUnitScope_Global, 0, &m_bufferSize, &outSize)
    //   , "couldn't get AudioUnit buffer size");
    
    MESCALINE_THROW_IF_ERROR(
        AudioUnitInitialize(m_rioUnit)
      , "couldn't initialize the remote I/O unit");

    // Set input and output format
    AudioStreamBasicDescription auFormat;
    const size_t sampleSize = sizeof(sample_t);
    
    // Input format
    memset(&auFormat, 0, sizeof(auFormat));
    auFormat.mSampleRate = m_sampleRate;
    auFormat.mFormatID = kAudioFormatLinearPCM;
    auFormat.mFormatFlags = kAudioFormatFlagsNativeFloatPacked;
    auFormat.mBitsPerChannel = 8 * sampleSize;
    auFormat.mChannelsPerFrame = m_numInputs;
    auFormat.mFramesPerPacket = 1;
    auFormat.mBytesPerPacket = sampleSize;
    auFormat.mBytesPerFrame = sampleSize;
    auFormat.mFormatFlags |= kAudioFormatFlagIsNonInterleaved;

    MESCALINE_THROW_IF_ERROR(
        AudioUnitSetProperty(m_rioUnit, kAudioUnitProperty_StreamFormat, kAudioUnitScope_Output, 1, &auFormat, sizeof(auFormat))
      , "couldn't set the remote I/O unit's input client format");

    // Output format
    memset(&auFormat, 0, sizeof(auFormat));
    auFormat.mSampleRate = m_sampleRate;
    auFormat.mFormatID = kAudioFormatLinearPCM;
    auFormat.mFormatFlags = kAudioFormatFlagsNativeFloatPacked;
    auFormat.mBitsPerChannel = 8 * sampleSize;
    auFormat.mChannelsPerFrame = m_numInputs;
    auFormat.mFramesPerPacket = 1;
    auFormat.mBytesPerPacket = sampleSize;
    auFormat.mBytesPerFrame = sampleSize;
    auFormat.mFormatFlags |= kAudioFormatFlagIsNonInterleaved;

    MESCALINE_THROW_IF_ERROR(
        AudioUnitSetProperty(m_rioUnit, kAudioUnitProperty_StreamFormat, kAudioUnitScope_Input, 0, &auFormat, sizeof(auFormat))
      , "couldn't set the remote I/O unit's output client format");
        
    // outSize = sizeof(outFormat);
    // MESCALINE_THROW_IF_ERROR(
    //     AudioUnitGetProperty(m_rioUnit, kAudioUnitProperty_StreamFormat, kAudioUnitScope_Output, 1, &outFormat, &outSize)
    //   , "couldn't get the remote I/O unit's output client format");
    // outFormat.Print();

    // Set render callback
    AURenderCallbackStruct inputProc;
    inputProc.inputProc = RenderCallback;
    inputProc.inputProcRefCon = this;

    MESCALINE_THROW_IF_ERROR(
        AudioUnitSetProperty(m_rioUnit, kAudioUnitProperty_SetRenderCallback, kAudioUnitScope_Input, 0, &inputProc, sizeof(inputProc))
      , "couldn't set remote i/o render callback");
    
    // Initialize I/O buffers
    m_inputBuffers = new float*[m_numInputs];
    m_outputBuffers = new float*[m_numOutputs];
    m_CAInputBuffers = new AudioBufferList[m_numInputs];
    m_CAInputBuffers->mNumberBuffers = m_numInputs;
    
    for (size_t i = 0; i < m_numInputs; i++) {
        m_inputBuffers[i] = Mescaline::Memory::allocAligned<sample_t>(Mescaline::Memory::Alignment::SIMDAlignment(), m_bufferSize);
        m_CAInputBuffers->mBuffers[i].mNumberChannels = 1;
        m_CAInputBuffers->mBuffers[i].mDataByteSize = m_bufferSize * sizeof(sample_t);
        m_CAInputBuffers->mBuffers[i].mData = m_inputBuffers[i];
    }
    
    // Initialize client
    client->configure(*this);
}

RemoteIODriver::~RemoteIODriver() throw (IO::Exception)
{
    // Free input buffer memory
    for (size_t i=0; i < m_numInputs; i++) {
        Mescaline::Memory::free(m_inputBuffers[i]);
    }
    // Free buffer pointer arrays
    delete [] m_inputBuffers;
    delete [] m_outputBuffers;
    delete [] m_CAInputBuffers;
}

void RemoteIODriver::start()
{
    MESCALINE_THROW_IF_ERROR(AudioOutputUnitStart(m_rioUnit), "couldn't start remote i/o unit");
}

void RemoteIODriver::stop()
{
    MESCALINE_THROW_IF_ERROR(AudioOutputUnitStop(m_rioUnit), "couldn't start remote i/o unit");
}

void RemoteIODriver::InterruptionCallback(void *inClientData, UInt32 inInterruption)
{
    RemoteIODriver* self = static_cast<RemoteIODriver*>(inClientData);

    if (inInterruption == kAudioSessionEndInterruption) {
        // make sure we are again the active session
        MESCALINE_THROW_IF_ERROR(AudioSessionSetActive(true), "couldn't set audio session active");
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
