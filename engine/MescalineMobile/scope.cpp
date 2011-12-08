#include "scope.hpp"

using namespace Mescaline::Audio;

ScopeSynth::ScopeSynth(MescalineHost* host, const Plugin::SynthDef<ScopeSynth>* synthDef)
{
}

void ScopeSynth::process(unsigned int numFrames, sample_t** inputs, sample_t** outputs)
{
    m_buffer.enqueue(inputs[0], numFrames);
}

MESCALINE_EXPORT void MESCALINE_INIT_FUNC(Scope)(MescalineHost* host)
{
    MescalineSynthDef* def = new ScopeSynthDef();
    MescalineHostRegisterSynthDef(host, def);
}
