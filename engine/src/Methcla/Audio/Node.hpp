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

#ifndef METHCLA_AUDIO_NODE_HPP_INCLUDED
#define METHCLA_AUDIO_NODE_HPP_INCLUDED

#include "Methcla/Audio/Resource.hpp"
#include "Methcla/Memory/Manager.hpp"
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
               , public Memory::Allocated<Node, Memory::RTMemoryManager>
               , public boost::intrusive::list_base_hook<boost::intrusive::link_mode<boost::intrusive::auto_unlink>>
    {
    protected:
        typedef Memory::Allocated<Node, Memory::RTMemoryManager> allocated_super;

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

        //* Return true if this node is a group.
        virtual bool isGroup() const { return false; }
        //* Return true if this node is a synth.
        virtual bool isSynth() const { return false; }

        //* Return the node's parent group or nullptr if it's the root node.
        const Group* parent() const { return m_parent; }
        Group* parent() { return m_parent; }

        //* Return true if the node is the root node.
        bool isRootNode() const { return parent() == 0; }

        // Process a number of frames.
        virtual void process(size_t numFrames) = 0;

    protected:
        Node(Environment& env, Group* target, AddAction addAction);

    private:
        Group* m_parent;
    };

    typedef boost::intrusive::list<Node,boost::intrusive::constant_time_size<false>> NodeList;
}; };

#endif // METHCLA_AUDIO_NODE_HPP_INCLUDED
