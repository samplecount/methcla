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

    class MetaData : public MescalineMetaData, boost::noncopyable
    {
    public:
        MetaData()
        {
            MescalineMetaDataInit(this);
        }
        ~MetaData()
        {
            MescalineAssoc* assoc = this->data;
            while (assoc != 0) {
                MescalineAssoc* next = assoc->next;
                delete assoc;
                assoc = next;
            }
        }

        void insert(const char* key, const char* value)
        {
            const char* key_ = newString(key);
            const char* value_ = newString(value);
            MescalineAssoc* assoc = new MescalineAssoc();
            MescalineAssocInitString(assoc, key_, value_);
            MescalineMetaDataInsert(this, assoc);
        }

        void insert(const char* key, int value)
        {
            const char* key_ = newString(key);
            MescalineAssoc* assoc = new MescalineAssoc();
            MescalineAssocInitInt(assoc, key_, value);
            MescalineMetaDataInsert(this, assoc);
        }

        void insert(const char* key, float value)
        {
            const char* key_ = newString(key);
            MescalineAssoc* assoc = new MescalineAssoc();
            MescalineAssocInitFloat(assoc, key_, value);
            MescalineMetaDataInsert(this, assoc);
        }

    private:
        const char* newString(const char* str)
        {
            m_strings.push_back(str);
            return m_strings.back().c_str();
        }

    private:
        std::vector<std::string> m_strings;
    };

    class ControlSpec : public MescalineControlSpec, boost::noncopyable
    {
    public:
        ControlSpec( MescalineControlFlags flags
                   , MetaData* metaData = 0 )
        {
            MescalineControlSpecInit(this);
            this->flags = flags;
            this->metaData = metaData;
        }
        ~ControlSpec()
        {
            delete this->metaData;
        }
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
        {
            // Initialize MescalineSynthDef fields
            MescalineSynthDefInit(this, m_name.c_str(), sizeof(T), boost::alignment_of<T>());
            this->numAudioInputs = numAudioInputs;
            this->numAudioOutputs = numAudioOutputs;
            this->numControlInputs = controlInputs.size();
            this->numControlOutputs = controlOutputs.size();
            this->metaData = metaData;
            this->fConstruct = &Construct;
            this->fDestroy = &Destroy;
            this->fGetControlInputSpec = &GetControlInputSpec;
            this->fGetControlOutputSpec = &GetControlOutputSpec;
            this->fGetUIDescription = &GetUIDescription;
            BOOST_FOREACH(ControlSpec* x, controlInputs)  { m_controlInputSpecs.push_back(x); }
            BOOST_FOREACH(ControlSpec* x, controlOutputs) { m_controlOutputSpecs.push_back(x); }
            BOOST_ASSERT( m_controlInputSpecs.size() == this->numControlInputs );
            BOOST_ASSERT( m_controlOutputSpecs.size() == this->numControlOutputs );
        }

        ~SynthDef()
        {
            delete this->metaData;
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
    };

}; }; };

#endif // MESCALINE_AUDIO_PLUGIN_SYNTHDEF_HPP_INCLUDED
