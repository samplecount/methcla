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

#ifndef METHCLA_AUDIO_IO_JACKDRIVER_HPP
#define METHCLA_AUDIO_IO_JACKDRIVER_HPP

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
        JackDriver() throw (IO::Exception);
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

#endif // METHCLA_AUDIO_IO_JACKDRIVER_HPP
