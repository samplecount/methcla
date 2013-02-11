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

#ifndef METHCLA_LILV_HPP_INCLUDED
#define METHCLA_LILV_HPP_INCLUDED

#include <boost/utility.hpp>
#include <memory>

#include "lilv/lilv.h"

namespace Methcla { namespace Lilv {

class Node : boost::noncopyable
{
public:
    Node(LilvNode* node);
    ~Node();

    const LilvNode* impl() const { return m_impl; }

private:
    LilvNode* m_impl;
};

typedef std::shared_ptr<Node> NodePtr;

class Nodes : boost::noncopyable
{
public:
    Nodes(LilvNodes* nodes);
    ~Nodes();
    
    const LilvNodes* impl() const { return m_impl; }

private:
    LilvNodes* m_impl;
};

typedef std::shared_ptr<Nodes> NodesPtr;

}; };

#endif // METHCLA_LILV_HPP_INCLUDED