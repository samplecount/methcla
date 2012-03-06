#ifndef MESCALINE_AUDIO_NODE_HPP_INCLUDED
#define MESCALINE_AUDIO_NODE_HPP_INCLUDED

#include <Mescaline/Audio.hpp>
#include <Mescaline/Memory/Manager.hpp>
#include <boost/cstdint.hpp>
#include <boost/intrusive/list.hpp>
#include <boost/utility.hpp>

namespace Mescaline { namespace Audio {

    typedef int32_t NodeId;
    // const NodeId InvalidNodeId = -1;

    class Environment;
    class Group;

    class Node : public Resource
               , public Memory::AllocatedBase<Node, Memory::RTMemoryManager>
               , public boost::intrusive::list_base_hook<>
    {
    public:
        Node(Environment& env, const ResourceId& id, Group* parent)
            : Resource(id)
            , m_env(env)
            , m_parent(parent)
        { }
        virtual ~Node();

        /// Return environment.
        const Environment& env() const { return m_env; }
        Environment& env() { return m_env; }
        
        const Group* parent() const { return m_parent; }
        Group* parent() { return m_parent; }
        bool isRootNode() const { return parent() == 0; }

        /// Free the node.
        virtual void free();

        // Process a number of frames.
        virtual void process(size_t numFrames) = 0;

    protected:
        void operator delete(void* ptr);

    private:
        Environment&    m_env;
        Group*          m_parent;
    };

    typedef boost::intrusive::list<Node> NodeList;
}; };

#endif // MESCALINE_AUDIO_NODE_HPP_INCLUDED
