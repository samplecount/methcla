// Copyright (C) 2012-2013 Samplecount S.L.
// Copyright (C) 2026 Methcla contributors
//
// SPDX-License-Identifier: Apache-2.0

#ifndef METHCLA_AUDIO_AUDIOBUS_HPP_INCLUDED
#define METHCLA_AUDIO_AUDIOBUS_HPP_INCLUDED

#include "Methcla/Audio.hpp"
#include "Methcla/Utility/StrongId.hpp"

namespace Methcla { namespace Audio {

    using AudioBusId =
        Methcla::Utility::StrongId<uint32_t, struct AudioBusIdTag>;

    class AudioBus
    {
    public:
        AudioBus(sample_t* data, Epoch epoch);
        virtual ~AudioBus();

        AudioBus(const AudioBus&) = delete;
        AudioBus& operator=(const AudioBus&) = delete;

        const Epoch& epoch() const
        {
            return m_epoch;
        }

        void setEpoch(const Epoch& epoch)
        {
            m_epoch = epoch;
        }

        sample_t* data()
        {
            return m_data;
        }

    protected:
        void setData(sample_t* data)
        {
            m_data = data;
        }

    private:
        Epoch     m_epoch;
        sample_t* m_data;
    };

    class ExternalAudioBus : public AudioBus
    {
    public:
        ExternalAudioBus(Epoch epoch);
        void setData(sample_t* data)
        {
            AudioBus::setData(data);
        }
    };

    class InternalAudioBus : public AudioBus
    {
    public:
        InternalAudioBus(size_t numFrames, Epoch epoch);
        virtual ~InternalAudioBus();
    };

}} // namespace Methcla::Audio

#endif // METHCLA_AUDIO_AUDIOBUS_HPP_INCLUDED
