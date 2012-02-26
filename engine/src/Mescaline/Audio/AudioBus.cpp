#include <Mescaline/Audio/AudioBus.hpp>
#include <Mescaline/Audio/Engine.hpp>

using namespace Mescaline::Audio;

AudioBus::AudioBus(size_t numFrames, sample_t* data, const Epoch& epoch)
: m_data(data)
, m_epoch(epoch)
{
}

AudioBus::~AudioBus()
{
}

ExternalAudioBus::ExternalAudioBus(size_t numFrames, const Epoch& epoch)
: AudioBus(numFrames, 0, epoch)
{
}

InternalAudioBus::InternalAudioBus(size_t numFrames, const Epoch& epoch)
: AudioBus( numFrames
          , allocAligned<sample_t>(Alignment::SIMDAlignment(), numFrames)
          , epoch )
{
}

InternalAudioBus::~InternalAudioBus()
{
    Mescaline::Memory::free(data());
}
