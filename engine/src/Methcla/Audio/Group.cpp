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

Group::Group(Environment& env, NodeId nodeId)
    : Node(env, nodeId)
    , m_first(nullptr)
    , m_last(nullptr)
{
}

Group::~Group()
{
    freeAll();
}

ResourceRef<Group> Group::construct(Environment& env, NodeId nodeId)
{
    return ResourceRef<Group>(new (env.rtMem().alloc(sizeof(Group))) Group(env, nodeId));
}

void Group::doProcess(size_t numFrames)
{
    Node* node = m_first;
    while (node != nullptr)
    {
        node->process(numFrames);
        node = node->m_next;
    }
}

void Group::addToHead(Node* node)
{
    assert(node != nullptr);
    assert(node->parent() == nullptr);
    assert(node->m_prev == nullptr);
    assert(node->m_next == nullptr);

    node->m_parent = this;
    node->m_next = m_first;

    if (m_first != nullptr)
        m_first->m_prev = node;

    m_first = node;

    if (m_last == nullptr)
        m_last = node;
}

void Group::addToTail(Node* node)
{
    assert(node != nullptr);
    assert(node->parent() == nullptr);
    assert(node->m_prev == nullptr);
    assert(node->m_next == nullptr);

    node->m_parent = this;
    node->m_prev = m_last;

    if (m_last != nullptr)
        m_last->m_next = node;

    m_last = node;

    if (m_first == nullptr)
        m_first = node;
}

void Group::addBefore(Node* target, Node* node)
{
    assert(target != nullptr);
    assert(node != nullptr);
    assert(target->parent() == this);
    assert(node->parent() == nullptr);
    assert(node->m_prev == nullptr);
    assert(node->m_next == nullptr);

    node->m_parent = this;
    node->m_prev = target->m_prev;
    target->m_prev = node;
    node->m_next = target;

    if (target == m_first)
        m_first = node;
}

void Group::addAfter(Node* target, Node* node)
{
    assert(target != nullptr);
    assert(node != nullptr);
    assert(target->parent() == this);
    assert(node->parent() == nullptr);
    assert(node->m_prev == nullptr);
    assert(node->m_next == nullptr);

    node->m_parent = this;
    node->m_next = target->m_next;
    target->m_next = node;
    node->m_prev = target;

    if (target == m_last)
        m_last = node;
}

void Group::remove(Node* node)
{
    assert(node != nullptr);
    assert(node->parent() == this);
    assert(   (node == m_first && node == m_last && node->m_prev == nullptr && node->m_next == nullptr)
           || !((node->m_prev == nullptr) && (node->m_next == nullptr)) );

    Node* prev = node->m_prev;
    Node* next = node->m_next;

    if (node == m_first)
    {
        assert( prev == nullptr );
        m_first = next;
        if (m_first != nullptr)
            m_first->m_prev = nullptr;
    }
    else
    {
        prev->m_next = next;
    }

    if (node == m_last)
    {
        assert( next == nullptr );
        m_last = prev;
        if (m_last != nullptr)
            m_last->m_next = nullptr;
    }
    else
    {
        next->m_prev = prev;
    }

    node->m_parent = nullptr;
    node->m_prev = nullptr;
    node->m_next = nullptr;
}

bool Group::isEmpty() const
{
    return m_first == nullptr;
}

void Group::freeAll()
{
    while (!isEmpty())
    {
        Node* node = m_first;
        remove(node);
        env().releaseNode(node->id());
    }
}
