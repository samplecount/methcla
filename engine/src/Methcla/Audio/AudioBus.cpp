#include <Methcla/Audio/AudioBus.hpp>
#include <Methcla/Audio/Engine.hpp>

using namespace Methcla::Audio;
using namespace Methcla::Memory;

AudioBus::AudioBus(Environment& env, const AudioBusId& id, size_t numFrames, sample_t* data, const Epoch& epoch)
    : Resource(env, id)
    , m_epoch(epoch)
    , m_data(data)
{
}

AudioBus::~AudioBus()
{
}

ExternalAudioBus::ExternalAudioBus(Environment& env, const AudioBusId& id, size_t numFrames, const Epoch& epoch)
    : AudioBus(env, id, numFrames, 0, epoch)
{
}

InternalAudioBus::InternalAudioBus(Environment& env, const AudioBusId& id, size_t numFrames, const Epoch& epoch)
    : AudioBus( env
              , id
              , numFrames
              , allocAlignedOf<sample_t,kSIMDAlignment>(numFrames)
              , epoch )
{
}

InternalAudioBus::~InternalAudioBus()
{
    Methcla::Memory::free(data());
}
