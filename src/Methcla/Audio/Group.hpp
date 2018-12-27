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

#include "Methcla/Audio/Node.hpp"

namespace Methcla { namespace Audio {

    class Group : public Node
    {
    public:
        const Node* first() const { return m_first; }
        Node*       first() { return m_first; }
        const Node* last() const { return m_last; }
        Node*       last() { return m_last; }

        static Group* construct(Environment& env, NodeId nodeId);

        virtual bool isGroup() const override { return true; }

        bool isEmpty() const;

        void addToHead(Node* node);
        void addToTail(Node* node);
        void addBefore(Node* target, Node* node);
        void addAfter(Node* target, Node* node);

        void freeAll();

    private:
        Group(Environment& env, NodeId nodeId);
        ~Group();

        virtual void doProcess(size_t numFrames) override;

    private:
        friend class Node;
        void remove(Node* node);

    private:
        Node* m_first;
        Node* m_last;
    };

}} // namespace Methcla::Audio

#endif // METHCLA_AUDIO_GROUP_HPP_INCLUDED
