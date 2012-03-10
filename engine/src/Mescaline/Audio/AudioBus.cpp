#include <Mescaline/Audio/AudioBus.hpp>
#include <Mescaline/Audio/Engine.hpp>

using namespace Mescaline::Audio;
using namespace Mescaline::Memory;

AudioBus::AudioBus(const ResourceId& id, size_t numFrames, sample_t* data, const Epoch& epoch)
    : Resource(id)
    , m_data(data)
    , m_epoch(epoch)
{
}

AudioBus::~AudioBus()
{
}

ExternalAudioBus::ExternalAudioBus(const ResourceId& id, size_t numFrames, const Epoch& epoch)
    : AudioBus(id, numFrames, 0, epoch)
{
}

InternalAudioBus::InternalAudioBus(const ResourceId& id, size_t numFrames, const Epoch& epoch)
    : AudioBus( id
              , numFrames
              , allocAlignedOf<sample_t,kSIMDAlignment>(numFrames)
              , epoch )
{
}

InternalAudioBus::~InternalAudioBus()
{
    Mescaline::Memory::free(data());
}
