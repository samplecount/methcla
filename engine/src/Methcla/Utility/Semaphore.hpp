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

#include <boost/utility.hpp>
#include "zix/sem.h"

namespace Methcla { namespace Utility {

class Semaphore : boost::noncopyable
{
public:
    Semaphore(unsigned initial=0)
    {
        zix_sem_init(&m_sem, initial);
    }
    ~Semaphore()
    {
        zix_sem_destroy(&m_sem);
    }

    void post() { zix_sem_post(&m_sem); }
    void wait() { zix_sem_wait(&m_sem); }
    void tryWait() { zix_sem_try_wait(&m_sem); }

private:
    ZixSem m_sem;
};

}; };

#endif // METHCLA_UTILITY_SEMAPHORE_HPP_INCLUDED
