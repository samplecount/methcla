#ifndef METHCLA_AUDIO_IO_CLIENT_H_INCLUDED
#define METHCLA_AUDIO_IO_CLIENT_H_INCLUDED

#include <Methcla/Audio.hpp>
#include <Methcla/Audio/IO/Driver.hpp>
#include <boost/cstdint.hpp>

namespace Methcla { namespace Audio { namespace IO
{
    class Client
    {
    public:
        virtual void configure(const Driver& driver) = 0;
        virtual void process(size_t numFrames, sample_t** inputs, sample_t** outputs) = 0;
    };
}; }; };

#endif // METHCLA_AUDIO_IO_CLIENT_H_INCLUDED
