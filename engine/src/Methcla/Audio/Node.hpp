#ifndef METHCLA_AUDIO_NODE_HPP_INCLUDED
#define METHCLA_AUDIO_NODE_HPP_INCLUDED

#include <Methcla/Audio/Resource.hpp>
#include <Methcla/Memory/Manager.hpp>
#include <boost/cstdint.hpp>
#include <boost/intrusive/list.hpp>
#include <boost/serialization/strong_typedef.hpp>
#include <boost/utility.hpp>

namespace Methcla { namespace Audio {

    BOOST_STRONG_TYPEDEF(uint32_t, NodeId);
    // const NodeId InvalidNodeId = -1;

    class Environment;
    class Group;

    class Node : public Resource<NodeId>
               , public Memory::Allocated<Node, Memory::RTMemoryManager, Memory::kSIMDAlignment>
               , public boost::intrusive::list_base_hook<>
    {
    protected:
        typedef Memory::Allocated<Node, Memory::RTMemoryManager, Memory::kSIMDAlignment> allocated_super;

    public:
        enum AddAction
        {
            kAddToHead
          , kAddToTail
          // , kAddBefore
          // , kAddAfter
          // , kReplace
        };

        virtual ~Node();
        //* Free a node.
        virtual void free();

        const Group* parent() const { return m_parent; }
        Group* parent() { return m_parent; }
        bool isRootNode() const { return parent() == 0; }

        // Process a number of frames.
        virtual void process(size_t numFrames) = 0;

    protected:
        Node(Environment& env, Group* target, AddAction addAction);

    private:
        Group* m_parent;
    };

    typedef boost::intrusive::list<Node> NodeList;
}; };

#endif // METHCLA_AUDIO_NODE_HPP_INCLUDED
