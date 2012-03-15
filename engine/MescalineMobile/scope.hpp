#ifndef MESCALINE_AUDIO_SCOPE_HPP_INCLUDED
#define MESCALINE_AUDIO_SCOPE_HPP_INCLUDED

#include <Mescaline/Audio/Plugin/API.hpp>
#include <boost/lockfree/ringbuffer.hpp>

namespace Mescaline { namespace Audio
{
    class ScopeSynth : public MescalineSynth
    {
    public:
        ScopeSynth(MescalineHost* host, const Plugin::SynthDef<ScopeSynth>* synthDef);
    
        void process(unsigned int numFrames, sample_t** inputs, sample_t** outputs);

        typedef boost::lockfree::ringbuffer<sample_t,1024> Buffer;
        Buffer& buffer() { return m_buffer; }

        float* controlInput(size_t index) { return 0; }
        float* controlOutput(size_t index) { return 0; }

    private:
        Buffer m_buffer;
    };
    
    class ScopeSynthDef : public Plugin::SynthDef<ScopeSynth>
    {
    public:
        ScopeSynthDef()
            : Plugin::SynthDef<ScopeSynth>("scope", 1, 0)
        { }
    };
}; };

#endif // MESCALNE_AUDIO_SCOPE_HPP_INCLUDED