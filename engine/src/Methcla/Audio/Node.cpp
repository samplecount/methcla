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

#include "Methcla/Audio/Engine.hpp"
#include "Methcla/Audio/Group.hpp"
#include "Methcla/Audio/Node.hpp"

using namespace Methcla::Audio;

Node::Node(Environment& env, NodeId nodeId)
    : Resource(env, nodeId)
    , m_parent(nullptr)
    , m_prev(nullptr)
    , m_next(nullptr)
    , m_doneFlags(kMethcla_NodeDoneDoNothing)
    , m_done(false)
{
}

Node::~Node()
{
    unlink();
}

void Node::unlink()
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
        env().freeNode(id());
    }
    else
    {
        doProcess(numFrames);
    }
}

void Node::free()
{
    Environment* pEnv = &env();
    this->~Node();
    pEnv->rtMem().free(this);
}

void Node::doProcess(size_t)
{
}

inline static void setDoneFreeSelf(Node* node)
{
    node->setDoneFlags((Methcla_NodeDoneFlags)(node->doneFlags() | kMethcla_NodeDoneFreeSelf));
    node->setDone();
}

void Node::setDone()
{
    Methcla_NodeDoneFlags flags(m_doneFlags);

    if (flags & kMethcla_NodeDoneFreeParent)
    {
        if (m_parent != nullptr)
            setDoneFreeSelf(m_parent);
    }
    else if (flags & kMethcla_NodeDoneFreeAllSiblings)
    {
        if (m_parent != nullptr)
        {
            Node* node = m_parent->m_first;
            while (node != this)
            {
                // env().freeNode(node->id());
                setDoneFreeSelf(node);
                node = node->m_next;
            }
            node = this->m_next;
            while (node != nullptr)
            {
                // env().freeNode(node->id());
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
                // env().freeNode(m_prev->id());
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
}