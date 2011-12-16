#ifndef MESCALINE_AUDIO_PLUGIN_SYNTHDEF_HPP_INCLUDED
#define MESCALINE_AUDIO_PLUGIN_SYNTHDEF_HPP_INCLUDED

#include <Mescaline/Audio/Plugin/API.h>
#include <Mescaline/Utility/Hash.hpp>

#include <boost/foreach.hpp>
#include <boost/ptr_container/ptr_vector.hpp>
#include <boost/type_traits.hpp>
#include <boost/unordered_map.hpp>
#include <boost/utility.hpp>

#include <string>
#include <vector>

namespace Mescaline { namespace Audio { namespace Plugin {

    class MetaData
    {
    public:
        void insert(const char* key, const char* value)
        {
            m_data.push_back(key);
            const char* keyData = m_data.back().c_str();
            m_data.push_back(value);
            const char* valueData = m_data.back().c_str();
            m_map[keyData] = valueData;
        }

        const char* lookup(const char* key)
        {
            Map::const_iterator it = m_map.find(key);
            return it == m_map.end() ? 0 : it->second;
        }

    private:
        typedef boost::unordered_map<
                    const char*
                  , const char*
                  , Mescaline::Utility::Hash::string_hash
                  , Mescaline::Utility::Hash::string_equal_to >
                Map;
        std::vector<std::string>    m_data;
        Map                         m_map;
    };

    class ControlSpec : public MescalineControlSpec
    {
    public:
        ControlSpec( const MescalineControlSpec& spec
                   , MetaData* metaData )
            : MescalineControlSpec(spec)
            , m_metaData(metaData)
        {
            this->fGetMetaData = &GetMetaData;
        }

        ControlSpec( MescalineControlFlags flags
                   , float minValue
                   , float maxValue
                   , float stepSize
                   , float defaultValue
                   , MetaData* metaData
                   )
            : m_metaData(metaData)
        {
            MescalineControlSpecInit(this);
            this->flags = flags;
            this->minValue = minValue;
            this->maxValue = maxValue;
            this->stepSize = stepSize;
            this->defaultValue = defaultValue;
            this->fGetMetaData = &GetMetaData;
        }
        ~ControlSpec()
        {
            delete m_metaData;
        }

    private:
        static const char* GetMetaData(const MescalineControlSpec* self, const char* key)
        {
            const ControlSpec* THIS = static_cast<const ControlSpec*>(self);
            return THIS->m_metaData == 0 ? 0 : THIS->m_metaData->lookup(key);
        }

    private:
        MetaData* m_metaData;
    };

    template <class T> class SynthDef : public MescalineSynthDef, boost::noncopyable
    {
    public:
        SynthDef( const char* name
                , size_t numAudioInputs
                , size_t numAudioOutputs
                , const std::vector<ControlSpec*>& controlInputs = std::vector<ControlSpec*>()
                , const std::vector<ControlSpec*>& controlOutputs = std::vector<ControlSpec*>()
                , MetaData* metaData = 0 )
            : m_name(name)
            , m_controlInputSpecs(controlInputs.size())
            , m_controlOutputSpecs(controlOutputs.size())
            , m_metaData(metaData)
        {
            // Initialize MescalineSynthDef fields
            MescalineSynthDefInit(this, m_name.c_str(), sizeof(T), boost::alignment_of<T>());
            this->numAudioInputs = numAudioInputs;
            this->numAudioOutputs = numAudioOutputs;
            this->numControlInputs = controlInputs.size();
            this->numControlOutputs = controlOutputs.size();
            this->fConstruct = &Construct;
            this->fDestroy = &Destroy;
            this->fGetControlInputSpec = &GetControlInputSpec;
            this->fGetControlOutputSpec = &GetControlOutputSpec;
            this->fGetUIDescription = &GetUIDescription;
            this->fGetMetaData = &GetMetaData;
            BOOST_FOREACH(ControlSpec* x, controlInputs)  { m_controlInputSpecs.push_back(x); }
            BOOST_FOREACH(ControlSpec* x, controlOutputs) { m_controlOutputSpecs.push_back(x); }
        }

        virtual ~SynthDef()
        {
            delete m_metaData;
        }

        const MescalineControlSpec* controlInputSpec(size_t index) const
            { return &m_controlInputSpecs[index]; }
        const MescalineControlSpec* controlOutputSpec(size_t index) const
            { return &m_controlOutputSpecs[index]; }

    private:
        static void Construct( MescalineHost* host
                             , const MescalineSynthDef* self
                             , MescalineSynth* instance )
        {
            instance->fProcess = &Process;
            instance->fGetControlInput = &GetControlInput;
            instance->fGetControlOutput = &GetControlOutput;
            new (instance) T(host, static_cast<const SynthDef<T>*>(self));
        }

        static void Destroy( MescalineHost*
                           , const MescalineSynthDef*
                           , MescalineSynth* instance )
        {
            static_cast<T*>(instance)->~T();
        }

        static void Process( MescalineSynth* instance
                           , size_t numFrames
                           , sample_t** inputs
                           , sample_t** outputs )
        {
            static_cast<T*>(instance)->process(numFrames, inputs, outputs);
        }

        static const MescalineControlSpec* GetControlInputSpec(const MescalineSynthDef* self, size_t index)
        {
            return static_cast<const SynthDef<T>*>(self)->controlInputSpec(index);
        }

        static const MescalineControlSpec* GetControlOutputSpec(const MescalineSynthDef* self, size_t index)
        {
            return static_cast<const SynthDef<T>*>(self)->controlOutputSpec(index);
        }

        static const MescalineUINode* GetUIDescription(const MescalineSynthDef* self)
        {
            return NULL;
        }

        static const char* GetMetaData(const MescalineSynthDef* self, const char* key)
        {
            const SynthDef<T>* THIS = static_cast<const SynthDef<T>*>(self);
            return THIS->m_metaData == NULL ? NULL : THIS->m_metaData->lookup(key);
        }

        static float* GetControlInput(MescalineSynth* instance, size_t index)
        {
            return static_cast<T*>(instance)->controlInput(index);
        }

        static float* GetControlOutput(MescalineSynth* instance, size_t index)
        {
            return static_cast<T*>(instance)->controlOutput(index);
        }

    private:
        typedef boost::ptr_vector<ControlSpec> ControlSpecs;

        std::string     m_name;
        ControlSpecs    m_controlInputSpecs;
        ControlSpecs    m_controlOutputSpecs;
        MetaData*       m_metaData;
    };

}; }; };

#endif // MESCALINE_AUDIO_PLUGIN_SYNTHDEF_HPP_INCLUDED
