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

using namespace Methcla::Audio;

#define METHCLA_ASSERT_NODE_IS_BLANK(node)   \
    BOOST_ASSERT(node != nullptr);           \
    BOOST_ASSERT(node->m_parent == nullptr); \
    BOOST_ASSERT(node->m_prev == nullptr);   \
    BOOST_ASSERT(node->m_next == nullptr);

#define METHCLA_ASSERT_NODE_IS_LINKED(node)                   \
    BOOST_ASSERT(m_first != nullptr);                         \
    BOOST_ASSERT(m_last != nullptr);                          \
    BOOST_ASSERT(node != nullptr);                            \
    BOOST_ASSERT(node->m_parent == this);                     \
    BOOST_ASSERT(node->m_prev != nullptr || node == m_first); \
    BOOST_ASSERT(node->m_next != nullptr || node == m_last);  \
    BOOST_ASSERT(m_first != m_last || (m_first == node && node == m_last));

Group::Group(Environment& env, NodeId nodeId)
: Node(env, nodeId)
, m_first(nullptr)
, m_last(nullptr)
{}

Group::~Group() { freeAll(); }

Group* Group::construct(Environment& env, NodeId nodeId)
{
    return new (env.rtMem().alloc(sizeof(Group))) Group(env, nodeId);
}

void Group::doProcess(size_t numFrames)
{
    Node* node = m_first;
    while (node != nullptr)
    {
        // Store pointer to next node because current node might be destroyed by
        // done action after process.
        Node* nextNode = node->m_next;
        node->process(numFrames);
        node = nextNode;
    }
}

void Group::addToHead(Node* node)
{
    METHCLA_ASSERT_NODE_IS_BLANK(node);

    node->m_parent = this;
    node->m_next = m_first;

    if (m_first != nullptr)
    {
        m_first->m_prev = node;
    }

    m_first = node;

    if (m_last == nullptr)
    {
        m_last = node;
    }

    METHCLA_ASSERT_NODE_IS_LINKED(node);
}

void Group::addToTail(Node* node)
{
    METHCLA_ASSERT_NODE_IS_BLANK(node);

    node->m_parent = this;
    node->m_prev = m_last;

    if (m_last != nullptr)
    {
        m_last->m_next = node;
    }

    m_last = node;

    if (m_first == nullptr)
    {
        m_first = node;
    }

    METHCLA_ASSERT_NODE_IS_LINKED(node);
}

void Group::addBefore(Node* target, Node* node)
{
    METHCLA_ASSERT_NODE_IS_LINKED(target);
    METHCLA_ASSERT_NODE_IS_BLANK(node);

    node->m_parent = this;
    node->m_prev = target->m_prev;
    target->m_prev = node;
    node->m_next = target;

    if (target == m_first)
    {
        BOOST_ASSERT(node->m_prev == nullptr);
        m_first = node;
    }
    else
    {
        BOOST_ASSERT(node->m_prev != nullptr);
        node->m_prev->m_next = node;
    }

    METHCLA_ASSERT_NODE_IS_LINKED(node);
}

void Group::addAfter(Node* target, Node* node)
{
    METHCLA_ASSERT_NODE_IS_LINKED(target);
    METHCLA_ASSERT_NODE_IS_BLANK(node);

    node->m_parent = this;
    node->m_next = target->m_next;
    target->m_next = node;
    node->m_prev = target;

    if (target == m_last)
    {
        BOOST_ASSERT(node->m_next == nullptr);
        m_last = node;
    }
    else
    {
        BOOST_ASSERT(node->m_next != nullptr);
        node->m_next->m_prev = node;
    }

    METHCLA_ASSERT_NODE_IS_LINKED(node);
}

void Group::remove(Node* node)
{
    METHCLA_ASSERT_NODE_IS_LINKED(node);

    if (node == m_first)
    {
        m_first = node->m_next;
        if (m_first != nullptr)
        {
            m_first->m_prev = nullptr;
        }
    }
    else
    {
        node->m_prev->m_next = node->m_next;
    }

    if (node == m_last)
    {
        m_last = node->m_prev;
        if (m_last != nullptr)
        {
            m_last->m_next = nullptr;
        }
    }
    else
    {
        node->m_next->m_prev = node->m_prev;
    }

    node->m_parent = nullptr;
    node->m_prev = nullptr;
    node->m_next = nullptr;
}

bool Group::isEmpty() const
{
    BOOST_ASSERT(m_first != nullptr || m_last == nullptr);
    return m_first == nullptr;
}

void Group::freeAll()
{
    Node* node = m_first;
    while (node != nullptr)
    {
        // Store pointer to next node because current node might be destroyed by
        // done action after process.
        Node* nextNode = node->m_next;
        node->free();
        node = nextNode;
    }
    BOOST_ASSERT(isEmpty());
}
