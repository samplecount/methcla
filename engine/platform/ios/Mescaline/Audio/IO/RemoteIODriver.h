#ifndef MESCALINE_AUDIO_IO_REMOTEIODRIVER_H_INCLUDED
#define MESCALINE_AUDIO_IO_REMOTEIODRIVER_H_INCLUDED

#include <Mescaline/Audio/IO/Client.h>
#include <Mescaline/Audio/IO/Driver.h>
#include <AudioUnit/AudioUnit.h>
#include <boost/cstdint.hpp>
#include "CAStreamBasicDescription.h"

namespace Mescaline { namespace Audio { namespace IO
{
    class RemoteIODriver : public Driver
    {
    public:
        RemoteIODriver(Client* client);
    	virtual ~RemoteIODriver();

        virtual double sampleRate() const { return m_sampleRate; }
        virtual size_t numInputs() const { return m_numInputs; }
        virtual size_t numOutputs() const { return m_numOutputs; }
        virtual size_t bufferSize() const { return m_bufferSize; }

        void start();
        void stop();

    private:
        static void InterruptionCallback(void *inClientData, UInt32 inInterruption);
        static OSStatus RenderCallback(void				*inRefCon, 
                                       AudioUnitRenderActionFlags 	*ioActionFlags, 
                                       const AudioTimeStamp         *inTimeStamp, 
                                       UInt32                       inBusNumber, 
                                       UInt32                       inNumberFrames, 
                                       AudioBufferList              *ioData);
    
    private:
        Client*             m_client;
        double              m_sampleRate;
        size_t              m_numInputs;
        size_t              m_numOutputs;
        size_t              m_bufferSize;
        AudioUnit           m_rioUnit;
    	float**				m_inputBuffers;
    	float**				m_outputBuffers;
        AudioBufferList*    m_CAInputBuffers;
    };
}; }; };

#endif // MESCALINE_AUDIO_IO_REMOTEIODRIVER_H_INCLUDED
