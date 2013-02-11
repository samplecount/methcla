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

#include "Methcla/Audio/AudioBus.hpp"
#include "Methcla/Audio/Engine.hpp"

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
