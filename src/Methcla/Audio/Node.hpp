// Copyright 2012-2013 Samplecount S.L.
//
// SPDX-License-Identifier: Apache-2.0

#ifndef METHCLA_AUDIO_NODE_HPP_INCLUDED
#define METHCLA_AUDIO_NODE_HPP_INCLUDED

#include "Methcla/Utility/StrongId.hpp"

#include <methcla/types.h>

#include <cstddef>
#include <cstdint>

namespace Methcla { namespace Audio {

    using NodeId = Methcla::Utility::StrongId<int32_t, struct NodeIdTag>;

    class Environment;
    class Group;

    class Node
    {
    public:
        Node(const Node&) = delete;
        Node(Node&&) = delete;
        Node& operator=(const Node&) = delete;

        //* Access environment.
        const Environment& env() const
        {
            return m_env;
        }
        Environment& env()
        {
            return m_env;
        }

        //* Return unique id.
        NodeId id() const
        {
            return m_id;
        }

        //* Return true if this node is a group.
        virtual bool isGroup() const
        {
            return false;
        }
        //* Return true if this node is a synth.
        virtual bool isSynth() const
        {
            return false;
        }

        //* Return the node's parent group or nullptr if it's the root node.
        const Group* parent() const
        {
            return m_parent;
        }
        Group* parent()
        {
            return m_parent;
        }

        const Node* prev() const
        {
            return m_prev;
        }
        Node* prev()
        {
            return m_prev;
        }

        const Node* next() const
        {
            return m_next;
        }
        Node* next()
        {
            return m_next;
        }

        // Process a number of frames.
        void process(size_t numFrames);

        Methcla_NodeDoneFlags doneFlags() const
        {
            return m_doneFlags;
        }

        void setDoneFlags(Methcla_NodeDoneFlags flags)
        {
            m_doneFlags = flags;
        }

        void setDone();

        //* Free a node.
        void free();

    protected:
        Node(Environment& env, NodeId nodeId);
        virtual ~Node();

        virtual void doProcess(size_t numFrames);

    protected:
        friend class Group;

        Environment& m_env;
        NodeId       m_id;

        Group* m_parent;
        Node*  m_prev;
        Node*  m_next;

        Methcla_NodeDoneFlags m_doneFlags;
        bool                  m_done;
    };
}} // namespace Methcla::Audio

#endif // METHCLA_AUDIO_NODE_HPP_INCLUDED
