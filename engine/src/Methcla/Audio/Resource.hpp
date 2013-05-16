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

#ifndef METHCLA_AUDIO_RESOURCE_HPP_INCLUDED
#define METHCLA_AUDIO_RESOURCE_HPP_INCLUDED

#include <boost/intrusive_ptr.hpp>
#include <boost/utility.hpp>
#include <stdexcept>
#include <vector>

namespace Methcla { namespace Audio
{
    class Environment;

    template<typename T>
    inline void intrusive_ptr_add_ref(T* expr)
    {
        expr->retain();
    }

    template<typename T>
    inline void intrusive_ptr_release(T* expr)
    {
        expr->release();
    }

    /// Resource base class.
    //
    // A resource has a reference to its environment and a unique id.
    template <class Id> class Resource : public boost::noncopyable
    {
    public:
        Resource(Environment& env, const Id& id)
            : m_env(env)
            , m_id(id)
            , m_references(0)
        { }
        virtual ~Resource()
        { }

        /// Return environment.
        const Environment& env() const { return m_env; }
        Environment& env() { return m_env; }

        /// Return unique id.
        const Id& id() const { return m_id; }

        inline void retain()
        {
            m_references++;
        }

        inline void release()
        {
            m_references--;
            BOOST_ASSERT_MSG( m_references >= 0 , "release() called once too many" );
            if (m_references == 0)
                this->free();
        }

    protected:
        virtual void free()
        {
            delete this;
        }

    private:
        Environment&    m_env;
        Id              m_id;
        int             m_references;
    };

    /// Simple map for holding pointers to resources.
    //
    // Also provides unique id allocation facility.
    template <class Id, class T> class ResourceMap : boost::noncopyable
    {
    public:
        typedef boost::intrusive_ptr<T> Pointer;

        ResourceMap(size_t size)
            : m_elems(size, nullptr)
        { }

        size_t size() const
        {
            return m_elems.size();
        }

        Id nextId()
        {
            for (size_t i=0; i < m_elems.size(); i++) {
                if (m_elems[i] == nullptr) {
                    return static_cast<Id>(i);
                }
            }
            throw std::runtime_error("No free ids");
        }

        void insert(const Id& id, Pointer a)
        {
            m_elems[id] = a;
        }

        void insert(const Id& id, T* a)
        {
            insert(id, Pointer(a));
        }

        void remove(const Id& id)
        {
            m_elems[id] = nullptr;
        }

        Pointer lookup(const Id& id) noexcept
        {
            if ((id >= Id(0)) && (id < Id(m_elems.size())))
                return m_elems[id];
            return nullptr;
        }

    private:
        std::vector<Pointer> m_elems;
    };
}; };

#endif // METHCLA_AUDIO_RESOURCE_HPP_INCLUDED
