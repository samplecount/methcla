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
#include <cassert>
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

    //* Reference counted base class
    class Reference
    {
    public:
        Reference()
            : m_refs(0)
        { }
        Reference(const Reference&) = delete;

        inline void retain() noexcept
        {
            m_refs++;
        }

        inline void release() noexcept
        {
            m_refs--;
            assert(m_refs >= 0);
            if (m_refs == 0)
                this->free();
        }

    protected:
        virtual ~Reference()
        { }

        virtual void free()
        {
            delete this;
        }

    private:
        int m_refs;
    };

    /// Resource base class.
    //
    // A resource has a reference to its environment and a unique id.
    template <typename Id> class Resource : public Reference
    {
    public:
        Resource(Environment& env, Id id)
            : m_env(env)
            , m_id(id)
        { }

        /// Access environment.
        const Environment& env() const { return m_env; }
        Environment& env() { return m_env; }

        /// Return unique id.
        Id id() const { return m_id; }

    private:
        Environment&    m_env;
        Id              m_id;
    };

    /// Simple map for holding pointers to resources.
    //
    // Also provides unique id allocation facility.
    template <typename Id, class T> class ResourceMap
    {
    public:
        typedef boost::intrusive_ptr<T> Pointer;

        ResourceMap(size_t size)
            : m_elems(size, nullptr)
        { }
        ResourceMap(const ResourceMap&) = delete;

        size_t size() const
        {
            return m_elems.size();
        }

        bool contains(Id id) const
        {
            return m_elems[id] != nullptr;
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

        void insert(Id id, Pointer a)
        {
            if ((id >= Id(0)) && (id < Id(m_elems.size()))) {
                m_elems[id] = a;
            } else {
                throw std::out_of_range("Invalid resource id");
            }
        }

        void insert(Id id, T* a)
        {
            insert(id, Pointer(a));
        }

        void remove(Id id)
        {
            m_elems[id] = nullptr;
        }

        Pointer lookup(Id id) noexcept
        {
            if ((id >= Id(0)) && (id < Id(m_elems.size())))
                return m_elems[id];
            return nullptr;
        }

    private:
        std::vector<Pointer> m_elems;
    };
} }

#endif // METHCLA_AUDIO_RESOURCE_HPP_INCLUDED
