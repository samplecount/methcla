#ifndef MESCALINE_AUDIO_IO_DRIVER_H_INCLUDED
#define MESCALINE_AUDIO_IO_DRIVER_H_INCLUDED

#include <Mescaline/Exception.hpp>
#include <boost/cstdint.hpp>

namespace Mescaline { namespace Audio { namespace IO
{
struct Exception : virtual Mescaline::Exception { };

class Driver
{
public:
    virtual double sampleRate() const = 0;
    virtual size_t numInputs() const = 0;
    virtual size_t numOutputs() const = 0;
    virtual size_t bufferSize() const = 0;

	virtual void start() = 0;
	virtual void stop() = 0;
};

}; }; };

#endif // MESCALINE_AUDIO_IO_DRIVER_H_INCLUDED