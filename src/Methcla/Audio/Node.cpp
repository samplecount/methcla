// Copyright (C) 2012-2013 Samplecount S.L.
// Copyright (C) 2026 Methcla contributors
//
// SPDX-License-Identifier: Apache-2.0

#include "Methcla/Audio/Engine.hpp"
#include "Methcla/Audio/Group.hpp"
#include "Methcla/Audio/Node.hpp"

#include <cassert>

using namespace Methcla::Audio;

Node::Node(Environment& env, NodeId nodeId)
: m_env(env)
, m_id(nodeId)
, m_parent(nullptr)
, m_prev(nullptr)
, m_next(nullptr)
, m_doneFlags(kMethcla_NodeDoneDoNothing)
, m_done(false)
{}

Node::~Node()
{
    if (m_parent)
    {
        m_parent->remove(this);
    }

    assert(m_parent == nullptr);
    assert(m_prev == nullptr);
    assert(m_next == nullptr);
}

void Node::process(size_t numFrames)
{
    if (m_done)
    {
        free();
    }
    else
    {
        doProcess(numFrames);
    }
}

void Node::free()
{
    Environment* pEnv = &env();
    // Send /node/ended notification
    pEnv->nodeEnded(id());
    this->~Node();
    pEnv->rtMem().free(this);
}

void Node::doProcess(size_t)
{}

inline static void setDoneFreeSelf(Node* node)
{
    node->setDoneFlags(
        (Methcla_NodeDoneFlags)(node->doneFlags() | kMethcla_NodeDoneFreeSelf));
    node->setDone();
}

void Node::setDone()
{
    Methcla_NodeDoneFlags flags(m_doneFlags);

    if (flags & kMethcla_NodeDoneFreeParent)
    {
        if (m_parent != nullptr && m_parent != env().rootNode())
        {
            setDoneFreeSelf(m_parent);
        }
    }
    else if (flags & kMethcla_NodeDoneFreeAllSiblings)
    {
        if (m_parent != nullptr)
        {
            Node* node = m_parent->m_first;
            while (node != this)
            {
                setDoneFreeSelf(node);
                node = node->m_next;
            }
            node = this->m_next;
            while (node != nullptr)
            {
                setDoneFreeSelf(node);
                node = node->m_next;
            }
        }
    }
    else
    {
        if (flags & kMethcla_NodeDoneFreePreceeding)
        {
            if (m_prev != nullptr)
                setDoneFreeSelf(m_prev);
        }
        if (flags & kMethcla_NodeDoneFreeFollowing)
        {
            if (m_next != nullptr)
                setDoneFreeSelf(m_next);
        }
        if (flags & kMethcla_NodeDoneFreeSelf)
        {
            m_done = true;
        }
    }

    if (flags & kMethcla_NodeDoneNotify)
    {
        env().notifyNodeDone(id());
    }
}
