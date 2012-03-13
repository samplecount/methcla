#ifndef Mescaline_Audio_Resource_hpp_included
#define Mescaline_Audio_Resource_hpp_included

#include <Mescaline/Audio/API.hpp>

#include <boost/assert.hpp>
#include <boost/intrusive/unordered_set.hpp>
#include <boost/intrusive_ptr.hpp>
#include <boost/utility.hpp>

namespace Mescaline { namespace Audio
{
    typedef uint32_t URID;

    class ResourceId
    {
    public:
        ResourceId()
            : m_id(InvalidId)
        { }
        explicit ResourceId(uint32_t id)
            : m_id(id)
        { BOOST_ASSERT( id != InvalidId ); }
        ResourceId(const ResourceId& other)
            : m_id(other.m_id)
        { }

        operator uint32_t() const
            { return m_id; }
        operator bool() const
            { return m_id != InvalidId; }

        friend bool operator==(const ResourceId& a, const ResourceId& b)
            { return a.m_id == b.m_id; }

        friend bool operator!=(const ResourceId& a, const ResourceId& b)
            { return a.m_id != b.m_id; }

        friend bool operator<(const ResourceId& a, const ResourceId& b)
            { return a.m_id < b.m_id; }

        friend bool operator>(const ResourceId& a, const ResourceId& b)
            { return a.m_id > b.m_id; }

        friend std::size_t hash_value(const ResourceId& x)
            { return std::size_t(x.m_id); }

    protected:
        static const uint32_t InvalidId = 0;

    private:
        uint32_t m_id;
    };

    class Environment;
    using namespace boost::intrusive;

    /// Resource class.
    //
    // Resources are /always/ created in the RT thread and can /only/
    // be retained or released from the RT thread.
    class Resource : public unordered_set_base_hook<>
                   , public boost::noncopyable
    {
    public:
        Resource(Environment& env, const ResourceId& id)
            : m_env(env)
            , m_id(id)
            , m_refCount(1)
        { }
        virtual ~Resource()
        { }

        /// Return environment.
        const Environment& env() const { return m_env; }
        Environment& env() { return m_env; }

//        virtual URID typeId() const = 0;
        const ResourceId& id() const { return m_id; }

        friend bool operator==(const Resource& a, const Resource& b)
            { return a.id() == b.id(); }

        friend std::size_t hash_value(const Resource& x)
            { return hash_value(x.id()); }

        typedef boost::intrusive_ptr<Resource> Handle;

		virtual void handleRequest(const API::Request& request);

    protected:
        /// Free the resource. This is called when the resource's reference count
        // drops to zero and should free the resource in the correct thread.
        //
        // For instance, Nodes are destroyed in the realtime thread, while AudioBuses
        // defer their destruction to the non-realtime thread.
        virtual void free();

    protected:
        friend class ResourceMap;
        friend void intrusive_ptr_add_ref(Resource*);
        friend void intrusive_ptr_release(Resource*);

        typedef uint32_t RefCount;

        RefCount refCount() const
        { return m_refCount; }

        void retain()
        { m_refCount++; }

        void release()
        {
            m_refCount--;
            if (m_refCount == 0) {
                this->free();
            }
        }

    private:
        Environment&    m_env;
        ResourceId      m_id;
        RefCount        m_refCount;
    };

    void intrusive_ptr_add_ref(Resource* x);
    void intrusive_ptr_release(Resource* x);

    class ResourceMap
    {
    public:
        ResourceMap()
            : m_map(bucket_traits(m_buckets, kNumBuckets))
        { }

        void insert(Resource& resource)
        {
            BOOST_ASSERT_MSG( resource.refCount() == 1, "Expected vanilla resource" );
            Map::insert_commit_data commit;
            std::pair<Map::iterator,bool> result = m_map.insert_check(resource.id(), KeyHasher(), KeyValueEqual(), commit);
            BOOST_ASSERT_MSG( result.second, "Duplicate resource ID" );
            m_map.insert_commit(resource, commit);
        }

        void remove(Resource& resource)
        {
            size_t n = m_map.erase(resource);
            BOOST_ASSERT_MSG( n > 0, "Invalid resource" );
            BOOST_ASSERT_MSG( n == 1, "Resource map inconsistency" );
            resource.release();
        }

        bool includes(const ResourceId& id)
        {
            return m_map.find(id, KeyHasher(), KeyValueEqual()) != m_map.end();
        }

        Resource::Handle lookup(const ResourceId& id)
        {
            Map::iterator it = m_map.find(id, KeyHasher(), KeyValueEqual());
            BOOST_ASSERT_MSG( it != m_map.end(), "Invalid resource ID" );
            return Resource::Handle(&*it);
        }

        ResourceId nextId()
        {
            // TODO: Find better algorithm.
            const uint32_t invalid = ResourceId();
            uint32_t id0 = m_map.size();
            uint32_t id = id0 + 1;
            while ((includes(ResourceId(id)) || id == invalid) && id != id0) {
                id++;
            }
            BOOST_ASSERT_MSG( id != id0, "No more free resource IDs" );
            return ResourceId(id);
        }

    private:
        typedef unordered_set<Resource>::bucket_type    bucket_type;
        typedef unordered_set<Resource>::bucket_traits  bucket_traits;
        typedef unordered_set<Resource, power_2_buckets<true> > Map;

        typedef boost::hash<ResourceId> KeyHasher;
        struct KeyValueEqual
        {
            bool operator()(const ResourceId& key, const Resource& value) const
            {
                return key == value.id();
            }
        };

        static const size_t kNumBuckets = 1024;
        // NOTE: Order is important here; m_buckets needs to be initialized
        //       before m_map in the constructor.
        bucket_type m_buckets[kNumBuckets];
        Map         m_map;
    };
}; };

#endif // Mescaline_Audio_Resource_hpp_included
