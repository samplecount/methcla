#ifndef MESCALINE_AUDIO_PLUGIN_SYNTHDEF_HPP_INCLUDED
#define MESCALINE_AUDIO_PLUGIN_SYNTHDEF_HPP_INCLUDED

#include <Mescaline/Audio/Plugin/API.h>
#include <boost/type_traits.hpp>
#include <string>

namespace Mescaline { namespace Audio { namespace Plugin {

    template <class T> class SynthDef : public MescalineSynthDef
    {
    public:
        SynthDef( const char* inName
                , size_t inNumAudioInputs
                , size_t inNumAudioOutputs
                , size_t inNumControlInputs
                , size_t inNumControlOutputs )
            : m_name(inName)
        {
            // Initialize MescalineSynthDef fields
            memset(this, 0, sizeof(MescalineSynthDef));
            name = m_name.c_str();
            instanceSize = sizeof(T);
            instanceAlignment = boost::alignment_of<T>();
            numAudioInputs = inNumAudioInputs;
            numAudioOutputs = inNumAudioOutputs;
            numControlInputs = inNumControlInputs;
            numControlOutputs = inNumControlOutputs;
            fConstruct = &Construct;
            fDestroy = &Destroy;
        }

    private:
        static void Construct( MescalineHost* host
                             , const MescalineSynthDef* self
                             , MescalineSynth* instance )
        {
            instance->fProcess = &Process;
            instance->fGetControlInput = &GetControlInput;
            instance->fGetControlOutput = &GetControlOutput;
            new (instance) T(host, reinterpret_cast<const SynthDef<T>*>(self));
        }

        static void Destroy( MescalineHost*
                           , const MescalineSynthDef*
                           , MescalineSynth* instance )
        {
            reinterpret_cast<T*>(instance)->~T();
        }
        
        static void Process( MescalineSynth* instance
                           , unsigned int numFrames
                           , sample_t** inputs
                           , sample_t** outputs )
        {
            reinterpret_cast<T*>(instance)->process(numFrames, inputs, outputs);
        }

        static float* GetControlInput(MescalineSynth* instance, size_t index)
        {
            return reinterpret_cast<T*>(instance)->controlInput(index);
        }

        static float* GetControlOutput(MescalineSynth* instance, size_t index)
        {
            return reinterpret_cast<T*>(instance)->controlOutput(index);
        }

    private:
        std::string m_name;
    };

}; }; };

#endif // MESCALINE_AUDIO_PLUGIN_SYNTHDEF_HPP_INCLUDED
