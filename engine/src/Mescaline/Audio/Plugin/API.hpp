#ifndef MESCALINE_AUDIO_PLUGIN_SYNTHDEF_HPP_INCLUDED
#define MESCALINE_AUDIO_PLUGIN_SYNTHDEF_HPP_INCLUDED

#include <Mescaline/Audio/Plugin/API.h>
#include <boost/type_traits.hpp>
#include <string>
#include <vector>

namespace Mescaline { namespace Audio { namespace Plugin {

    class Warp : public MescalineWarp
    {
    public:
        typedef float (*Function)(const MescalineControlSpec*, float);

        Warp(MescalineWarpType warp=kMescalineWarpLinear, float curve=0)
        {
            type = warp;
            data.curve = curve;
            data.function.fMap = 0;
            data.function.fUnmap = 0;
        }
        Warp(float curve)
        {
            type = kMescalineWarpCurve;
            data.curve = curve;
            data.function.fMap = 0;
            data.function.fUnmap = 0;
        }
        Warp(Function map, Function unmap)
        {
            type = kMescalineWarpFunction;
            data.curve = 0;
            data.function.fMap = map;
            data.function.fUnmap = unmap;
        }
    };

    class ControlSpec : public MescalineControlSpec
    {
    public:
        ControlSpec( const char* name = ""
                   , float minValue = 0
                   , float maxValue = 0
                   , float stepSize = 0
                   , float defaultValue=0
                   , MescalineControlFlags flags = kMescalineNoFlags
                   , const Warp& warp = Warp()
                   )
            : m_name(name)
        {
            MescalineControlSpec::name = m_name.c_str();
            MescalineControlSpec::minValue = minValue;
            MescalineControlSpec::maxValue = maxValue;
            MescalineControlSpec::stepSize = stepSize;
            MescalineControlSpec::defaultValue = defaultValue;
            MescalineControlSpec::flags = flags;
            MescalineControlSpec::warp = warp;
        }
                   
    private:
        std::string m_name;
    };

    template <class T> class SynthDef : public MescalineSynthDef
    {
    public:
        SynthDef( const char* inName
                , size_t inNumAudioInputs
                , size_t inNumAudioOutputs
                , size_t inNumControlInputs
                , size_t inNumControlOutputs )
            : m_name(inName)
            , m_controlInputSpecs(inNumControlInputs)
            , m_controlOutputSpecs(inNumControlOutputs)
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

        void addControlInputSpec(size_t index, const ControlSpec& spec)
            { m_controlInputSpecs[index] = spec; }
        void addControlOutputSpec(size_t index, const ControlSpec& spec)
            { m_controlOutputSpecs[index] = spec; }
        const MescalineControlSpec* controlInputSpec(size_t index) const
            { return m_controlInputSpecs[index]; }
        const MescalineControlSpec* controlOutputSpec(size_t index) const
            { return m_controlOutputSpecs[index]; }
        
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

		static MescalineControlSpec* GetControlInputSpec(const MescalineSynthDef* self, size_t index)
		{
            return reinterpret_cast<const SynthDef<T>*>(self)->controlInputSpec(index);
		}

		static MescalineControlSpec* GetControlOutputSpec(const MescalineSynthDef* self, size_t index)
		{
            return reinterpret_cast<const SynthDef<T>*>(self)->controlOutputSpec(index);
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
        std::vector<ControlSpec> m_controlInputSpecs;
        std::vector<ControlSpec> m_controlOutputSpecs;
    };

}; }; };

#endif // MESCALINE_AUDIO_PLUGIN_SYNTHDEF_HPP_INCLUDED
