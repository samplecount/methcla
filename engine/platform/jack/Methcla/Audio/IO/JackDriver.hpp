#ifndef Methcla_Audio_IO_JackDriver_hpp
#define Methcla_Audio_IO_JackDriver_hpp

#include "Methcla/Audio/IO/Client.hpp"
#include "Methcla/Audio/IO/Driver.hpp"
#include "Methcla/Audio/Plugin/Types.h"
#include "Methcla/Memory.hpp"

#include <jack/jack.h>

#include <boost/cstdint.hpp>
#include <boost/exception/all.hpp>

namespace Methcla { namespace Audio { namespace IO
{
    class JackDriver : public Driver
    {
    public:
        JackDriver(Client* client) throw (IO::Exception);
        virtual ~JackDriver();

        virtual double sampleRate() const { return m_sampleRate; }
        virtual size_t numInputs() const { return m_numInputs; }
        virtual size_t numOutputs() const { return m_numOutputs; }
        virtual size_t bufferSize() const { return m_bufferSize; }

        virtual void start();
        virtual void stop();

    private:
        static int sampleRateCallback(jack_nframes_t nframes, void* arg);
        static int bufferSizeCallback(jack_nframes_t nframes, void* arg);
        static int processCallback(jack_nframes_t nframes, void* arg);

        // static void InterruptionCallback(void *inClientData, UInt32 inInterruption);
        // static OSStatus RenderCallback(void                         *inRefCon, 
        //                                AudioUnitRenderActionFlags   *ioActionFlags, 
        //                                const AudioTimeStamp         *inTimeStamp, 
        //                                UInt32                       inBusNumber, 
        //                                UInt32                       inNumberFrames, 
        //                                AudioBufferList              *ioData);
    
    private:
        double              m_sampleRate;
        size_t              m_numInputs;
        size_t              m_numOutputs;
        size_t              m_bufferSize;
        jack_client_t*      m_jackClient;
        jack_port_t**       m_jackInputPorts;
        jack_port_t**       m_jackOutputPorts;
        sample_t**          m_inputBuffers;
        sample_t**          m_outputBuffers;
        // AudioBufferList*    m_CAInputBuffers;
    };
}; }; };

#endif // Methcla_Audio_IO_JackDriver_hpp
