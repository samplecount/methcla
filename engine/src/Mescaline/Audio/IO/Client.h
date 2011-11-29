#ifndef MESCALINE_AUDIO_IO_CLIENT_H_INCLUDED
#define MESCALINE_AUDIO_IO_CLIENT_H_INCLUDED

#include <Mescaline/Audio.h>
#include <Mescaline/Audio/IO/Driver.h>
#include <boost/cstdint.hpp>

namespace Mescaline { namespace Audio { namespace IO
{
    class Client
    {
    public:
        virtual void configure(const Driver& driver) = 0;
        virtual void process(size_t numFrames, sample_t** inputs, sample_t** outputs) = 0;
    };
}; }; };

#endif // MESCALINE_AUDIO_IO_CLIENT_H_INCLUDED
