#ifndef Methcla_Audio_IO_RemoteIODriver_hpp
#define Methcla_Audio_IO_RemoteIODriver_hpp

#include "Methcla/Audio/IO/Client.hpp"
#include "Methcla/Audio/IO/Driver.hpp"
#include "Methcla/Audio/Types.h"
#include "Methcla/Exception.hpp"
#include "Methcla/Memory.hpp"

#include <AudioUnit/AudioUnit.h>

#include <boost/cstdint.hpp>
#include <boost/exception/all.hpp>

namespace Methcla { namespace Audio { namespace IO
{
	struct OSStatusInfoTag { };
	typedef boost::error_info<OSStatusInfoTag,OSStatus> OSStatusInfo;

    class RemoteIODriver : public Driver
    {
    public:
        RemoteIODriver(Client* client) throw (IO::Exception);
        virtual ~RemoteIODriver();

        virtual double sampleRate() const { return m_sampleRate; }
        virtual size_t numInputs() const { return m_numInputs; }
        virtual size_t numOutputs() const { return m_numOutputs; }
        virtual size_t bufferSize() const { return m_bufferSize; }

        virtual void start();
        virtual void stop();

    private:
        static void InterruptionCallback(void *inClientData, UInt32 inInterruption);
        static OSStatus InputCallback(void                         *inRefCon,
                                      AudioUnitRenderActionFlags   *ioActionFlags,
                                      const AudioTimeStamp         *inTimeStamp,
                                      UInt32                       inBusNumber,
                                      UInt32                       inNumberFrames,
                                      AudioBufferList              *ioData);
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
    };
}; }; };

#endif // Methcla_Audio_IO_RemoteIODriver_hpp
