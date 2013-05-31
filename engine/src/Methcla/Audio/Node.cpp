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

Node::Node(Environment& env, NodeId nodeId, Group* target, AddAction addAction)
    : Resource(env, nodeId)
{
    if (target == nullptr) {
        m_parent = nullptr;
    } else {
        switch (addAction) {
            case kAddToHead:
                m_parent = target;
                target->addToHead(*this);
                break;
            case kAddToTail:
                m_parent = target;
                target->addToTail(*this);
                break;
        }
    }
}

Node::~Node()
{
#if 0
    std::cout << "~Node " << id() << "\n";
#endif
}

void Node::process(size_t numFrames)
{
    doProcess(numFrames);
}

void Node::free()
{
    delete this;
}

void Node::doProcess(size_t numFrames)
{
}
