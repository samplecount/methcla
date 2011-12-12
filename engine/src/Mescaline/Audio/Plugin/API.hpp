#ifndef MESCALINE_AUDIO_PLUGIN_SYNTHDEF_HPP_INCLUDED
#define MESCALINE_AUDIO_PLUGIN_SYNTHDEF_HPP_INCLUDED

#include <Mescaline/Audio/Plugin/API.h>
#include <boost/ptr_container/ptr_vector.hpp>
#include <boost/type_traits.hpp>
#include <boost/utility.hpp>
#include <string>
#include <vector>

namespace Mescaline { namespace Audio { namespace Plugin {

    class MetaData : public MescalineMetaData
    {
    public:
        MetaData(const char* key, const char* value)
        {
            MescalineMetaDataInit(this, key, value);
        }

        MetaData(const MetaData& list, const char* key, const char* value)
        {
            MescalineMetaDataInit(this, key, value);
            MescalineMetaDataCons(this, &list);
        }
        ~MetaData()
        {
            if (next) delete next;
        }

        class const_iterator
        {
        public:
            const_iterator(const MetaData* list)
                : m_list(list)
            { }
            
            const_iterator& operator++()
            {
                if (m_list) {
                    m_list = reinterpret_cast<const MetaData*>(m_list->next);
                }
                return *this;
            }
            
            const MetaData& operator->()
            {
                return *m_list;
            }

        private:
            const MetaData* m_list;
        };
        
        const_iterator begin() const { return const_iterator(this); }
        const_iterator end() const { return const_iterator(0); }
    };

    class Mapping : public MescalineMapping
    {
    public:
        Mapping(MescalineMappingFunction function=kMescalineMappingLinear, float curve=0)
        {
            this->function = function;
            this->params.curve = curve;
        }
    };

    class ControlSpec : public MescalineControlSpec
    {
    public:
        ControlSpec( MescalineControlFlags flags = kMescalineNoFlags
                   , float minValue = 0
                   , float maxValue = 0
                   , float stepSize = 0
                   , float defaultValue=0
                   , const Mapping& mapping = Mapping()
                   )
        {
            MescalineControlSpecInit(this);
            this->flags = flags;
            this->minValue = minValue;
            this->maxValue = maxValue;
            this->stepSize = stepSize;
            this->defaultValue = defaultValue;
            this->mapping = mapping;
        }
        ~ControlSpec()
        {
            delete metaData();
        }
        
        void setMetaData(const MetaData* metaData) { MescalineControlSpec::metaData = reinterpret_cast<const MescalineMetaData*>(metaData); }
        const MetaData* metaData() const { return reinterpret_cast<const MetaData*>(MescalineControlSpec::metaData); }
    };

    template <class T> class SynthDef : public MescalineSynthDef, boost::noncopyable
    {
    public:
        SynthDef( const char* name
                , size_t numAudioInputs
                , size_t numAudioOutputs
                , size_t numControlInputs
                , size_t numControlOutputs )
            : m_name(name)
            , m_controlInputSpecs(numControlInputs)
            , m_controlOutputSpecs(numControlOutputs)
        {
            // Initialize MescalineSynthDef fields
            MescalineSynthDefInit(this, m_name.c_str(), sizeof(T), boost::alignment_of<T>());
            this->numAudioInputs = numAudioInputs;
            this->numAudioOutputs = numAudioOutputs;
            this->numControlInputs = numControlInputs;
            this->numControlOutputs = numControlOutputs;
            this->fConstruct = &Construct;
            this->fDestroy = &Destroy;
            this->fGetControlInputSpec = GetControlInputSpec;
            this->fGetControlOutputSpec = GetControlOutputSpec;
            this->fGetUIDescription = GetUIDescription;
        }

        const MescalineControlSpec* controlInputSpec(size_t index) const
            { return &m_controlInputSpecs[index]; }
        const MescalineControlSpec* controlOutputSpec(size_t index) const
            { return &m_controlOutputSpecs[index]; }

    protected:
        void addControlInputSpec(ControlSpec* spec)
            { m_controlInputSpecs.push_back(spec); }
        void addControlOutputSpec(ControlSpec* spec)
            { m_controlOutputSpecs.push_back(spec); }

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
                           , size_t numFrames
                           , sample_t** inputs
                           , sample_t** outputs )
        {
            reinterpret_cast<T*>(instance)->process(numFrames, inputs, outputs);
        }

		static const MescalineControlSpec* GetControlInputSpec(const MescalineSynthDef* self, size_t index)
		{
            return reinterpret_cast<const SynthDef<T>*>(self)->controlInputSpec(index);
		}

		static const MescalineControlSpec* GetControlOutputSpec(const MescalineSynthDef* self, size_t index)
		{
            return reinterpret_cast<const SynthDef<T>*>(self)->controlOutputSpec(index);
		}

        static const MescalineUINode* GetUIDescription(const MescalineSynthDef* self)
        {
            return NULL;
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
        typedef boost::ptr_vector<ControlSpec> ControlSpecs;
        ControlSpecs m_controlInputSpecs;
        ControlSpecs m_controlOutputSpecs;
    };

}; }; };

#endif // MESCALINE_AUDIO_PLUGIN_SYNTHDEF_HPP_INCLUDED
