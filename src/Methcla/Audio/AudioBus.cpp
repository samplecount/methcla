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

AudioBus::AudioBus(sample_t* data, Epoch epoch)
: m_epoch(epoch)
, m_data(data)
{}

AudioBus::~AudioBus() {}

ExternalAudioBus::ExternalAudioBus(Epoch epoch)
: AudioBus(nullptr, epoch)
{}

InternalAudioBus::InternalAudioBus(size_t numFrames, Epoch epoch)
: AudioBus(allocAlignedOf<sample_t>(kSIMDAlignment, numFrames), epoch)
{}

InternalAudioBus::~InternalAudioBus() { Methcla::Memory::freeAligned(data()); }
