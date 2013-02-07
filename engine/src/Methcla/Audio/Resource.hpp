#ifndef Methcla_Audio_Resource_hpp_included
#define Methcla_Audio_Resource_hpp_included

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
            return m_elems[id];
        }

    private:
        std::vector<T*> m_elems;
    };
}; };

#endif // Methcla_Audio_Resource_hpp_included
