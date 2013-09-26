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

#ifndef METHCLA_UTILITY_SEMAPHORE_HPP_INCLUDED
#define METHCLA_UTILITY_SEMAPHORE_HPP_INCLUDED

namespace Methcla { namespace Utility {

namespace detail { struct SemaphoreImpl; }

class Semaphore
{
public:
    Semaphore(unsigned initial=0);
    ~Semaphore();

    Semaphore(const Semaphore&) = delete;
    Semaphore& operator=(const Semaphore&) = delete;

    void post();
    void wait();
    bool tryWait();

private:
    struct detail::SemaphoreImpl* m_impl;
};

} }

#endif // METHCLA_UTILITY_SEMAPHORE_HPP_INCLUDED
