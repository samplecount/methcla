#include <Mescaline/Audio/AudioBus.hpp>
#include <Mescaline/Audio/Engine.hpp>

using namespace Mescaline::Audio;
using namespace Mescaline::Memory;

AudioBus::AudioBus(Environment& env, const ResourceId& id, size_t numFrames, sample_t* data, const Epoch& epoch)
    : Resource(env, id)
    , m_data(data)
    , m_epoch(epoch)
{
}

AudioBus::~AudioBus()
{
}

void AudioBus::free()
{
    Command* cmd = new (env().rtMem()) DeferredDeleteCommand<AudioBus>(env(), this);
    // FIXME: How to statically ensure that this can only be called from the RT thread?
    env().enqueue(kRealtime, cmd);
}

ExternalAudioBus::ExternalAudioBus(Environment& env, const ResourceId& id, size_t numFrames, const Epoch& epoch)
    : AudioBus(env, id, numFrames, 0, epoch)
{
}

InternalAudioBus::InternalAudioBus(Environment& env, const ResourceId& id, size_t numFrames, const Epoch& epoch)
    : AudioBus( env
              , id
              , numFrames
              , allocAlignedOf<sample_t,kSIMDAlignment>(numFrames)
              , epoch )
{
}

InternalAudioBus::~InternalAudioBus()
{
    Mescaline::Memory::free(data());
}
