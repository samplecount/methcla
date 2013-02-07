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

#ifndef METHCLA_AUDIO_GROUP_HPP_INCLUDED
#define METHCLA_AUDIO_GROUP_HPP_INCLUDED

#include <Methcla/Audio/Node.hpp>

namespace Methcla { namespace Audio {

class Group : public Node
{
protected:
    Group(Environment& env, Group* target, Node::AddAction addAction)
        : Node(env, target, addAction)
    { }

public:
    static Group* construct(Environment& env, Group* target, AddAction addAction);
    virtual void free() override;

    const NodeList& children() const { return m_children; }
    void addToHead(Node& node) { m_children.push_front(node); }
    void addToTail(Node& node) { m_children.push_back(node); }

    virtual void process(size_t numFrames) override;

private:
    NodeList m_children;
};

}; };

#endif // METHCLA_AUDIO_GROUP_HPP_INCLUDED
