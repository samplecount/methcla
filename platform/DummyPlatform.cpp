#include "Methcla/Audio/IO/DummyDriver.hpp"
#include "Methcla/Platform.hpp"

Methcla::Audio::IO::Driver* Methcla::Platform::defaultAudioDriver(
    Methcla::Audio::IO::Driver::Options options)
{
    return new Methcla::Audio::IO::DummyDriver(options);
}
