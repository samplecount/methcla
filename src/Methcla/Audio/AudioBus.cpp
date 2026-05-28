// Copyright 2012-2013 Samplecount S.L.
//
// SPDX-License-Identifier: Apache-2.0

#include "Methcla/Audio/AudioBus.hpp"
#include "Methcla/Audio/Engine.hpp"

using namespace Methcla::Audio;
using namespace Methcla::Memory;

AudioBus::AudioBus(sample_t* data, Epoch epoch)
: m_epoch(epoch)
, m_data(data)
{}

AudioBus::~AudioBus()
{}

ExternalAudioBus::ExternalAudioBus(Epoch epoch)
: AudioBus(nullptr, epoch)
{}

InternalAudioBus::InternalAudioBus(size_t numFrames, Epoch epoch)
: AudioBus(allocAlignedOf<sample_t>(kSIMDAlignment, numFrames), epoch)
{}

InternalAudioBus::~InternalAudioBus()
{
    Methcla::Memory::freeAligned(data());
}
