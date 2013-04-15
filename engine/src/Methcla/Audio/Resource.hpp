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

#include <boost/utility.hpp>
#include <stdexcept>
#include <vector>

namespace Methcla { namespace Audio
{
    class Environment;

    /// Resource base class.
    //
    // A resource has a reference to its environment and a unique id.
    template <class Id> class Resource : public boost::noncopyable
    {
    public:
        Resource(Environment& env, const Id& id)
            : m_env(env)
            , m_id(id)
        { }
        virtual ~Resource()
        { }

        /// Return environment.
        const Environment& env() const { return m_env; }
        Environment& env() { return m_env; }

        /// Return unique id.
        const Id& id() const { return m_id; }

    private:
        Environment&    m_env;
        Id              m_id;
    };

    /// Simple map for holding pointers to resources.
    //
    // Also provides unique id allocation facility.
    template <class Id, class T> class ResourceMap : boost::noncopyable
    {
    public:
        ResourceMap(size_t size)
            : m_elems(size, nullptr)
        { }
        ~ResourceMap()
        {
            for (T*& a : m_elems) {
                delete a;
            }
        }

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

        void insert(const Id& id, T* a)
        {
            m_elems[id] = a;
        }

        void remove(const Id& id)
        {
            m_elems[id] = nullptr;
        }

        T* lookup(const Id& id)
        {
            if ((id >= Id(0)) && (id < m_elems.size()))
                return m_elems[id];
            throw std::invalid_argument("Invalid resource ID");
        }

    private:
        std::vector<T*> m_elems;
    };
}; };

#endif // METHCLA_AUDIO_RESOURCE_HPP_INCLUDED
