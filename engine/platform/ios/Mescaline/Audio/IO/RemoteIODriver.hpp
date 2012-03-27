#ifndef Mescaline_Audio_IO_RemoteIODriver_hpp
#define Mescaline_Audio_IO_RemoteIODriver_hpp

#include "Mescaline/Audio/IO/Client.hpp"
#include "Mescaline/Audio/IO/Driver.hpp"
#include "Mescaline/Audio/Plugin/Types.h"
#include "Mescaline/Memory.hpp"

#include <AudioUnit/AudioUnit.h>

#include <boost/cstdint.hpp>
#include <boost/exception/all.hpp>

namespace Mescaline { namespace Audio { namespace IO
{
    class RemoteIODriver : public Driver
    {
    public:
        RemoteIODriver(Client* client) throw (IO::Exception);
        virtual ~RemoteIODriver() throw (IO::Exception);

        virtual double sampleRate() const { return m_sampleRate; }
        virtual size_t numInputs() const { return m_numInputs; }
        virtual size_t numOutputs() const { return m_numOutputs; }
        virtual size_t bufferSize() const { return m_bufferSize; }

        virtual void start();
        virtual void stop();

    private:
        static void InterruptionCallback(void *inClientData, UInt32 inInterruption);
        static OSStatus RenderCallback(void                         *inRefCon, 
                                       AudioUnitRenderActionFlags   *ioActionFlags, 
                                       const AudioTimeStamp         *inTimeStamp, 
                                       UInt32                       inBusNumber, 
                                       UInt32                       inNumberFrames, 
                                       AudioBufferList              *ioData);
    
    private:
        double              m_sampleRate;
        size_t              m_numInputs;
        size_t              m_numOutputs;
        size_t              m_bufferSize;
        AudioUnit           m_rioUnit;
        sample_t**          m_inputBuffers;
        sample_t**          m_outputBuffers;
        AudioBufferList*    m_CAInputBuffers;
    };
}; }; };

#endif // Mescaline_Audio_IO_RemoteIODriver_hpp
