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

#include "Methcla/Utility/Semaphore.hpp"

#if !METHCLA_USE_CV_SEMAPHORE

#include "Methcla/Exception.hpp"
#include "zix/sem.h"

#include <memory>
#include <stdexcept>

using namespace Methcla;
using namespace Methcla::Utility;

class Methcla::Utility::detail::SemaphoreImpl : public ZixSem { };

static void check(ZixStatus status)
{
    switch (status) {
        case ZIX_STATUS_SUCCESS:
            break;
        case ZIX_STATUS_ERROR:
            throw Error(kMethcla_UnspecifiedError);
        case ZIX_STATUS_NO_MEM:
            throw std::bad_alloc();
        case ZIX_STATUS_NOT_FOUND:
            throw Error(kMethcla_FileNotFoundError);
        case ZIX_STATUS_EXISTS:
            throw Error(kMethcla_FileExistsError);
        case ZIX_STATUS_BAD_ARG:
            throw Error(kMethcla_ArgumentError);
        case ZIX_STATUS_BAD_PERMS:
            throw Error(kMethcla_PermissionsError);
    }
}

Semaphore::Semaphore(unsigned initial)
{
    m_impl = new detail::SemaphoreImpl;
    ZixStatus status = zix_sem_init(m_impl, initial);
    check(status);
}

Semaphore::~Semaphore()
{
    zix_sem_destroy(m_impl);
    delete m_impl;
}

void Semaphore::post()
{
    zix_sem_post(m_impl);
}

void Semaphore::wait()
{
    ZixStatus status = zix_sem_wait(m_impl);
    check(status);
}

bool Semaphore::tryWait()
{
    return zix_sem_try_wait(m_impl);
}

#else // !METHCLA_USE_CV_SEMAPHORE
#include <mutex>
#include <condition_variable>

namespace Methcla { namespace Utility {

    namespace detail
    {
        class SemaphoreImpl
        {
            std::mutex m_mutex;
            std::condition_variable m_condVar;
            unsigned m_count;

        public:
            SemaphoreImpl(unsigned count)
                : m_count(count)
            {}

            void post()
            {
                std::lock_guard<std::mutex> lock(m_mutex);
                m_count++;
                m_condVar.notify_one();
            }

            void wait()
            {
                std::unique_lock<std::mutex> lock(m_mutex);

                while (m_count == 0) {
                    m_condVar.wait(lock);
                }

                m_count--;
            }

            bool tryWait()
            {
                std::unique_lock<std::mutex> lock(m_mutex);

                if (m_count > 0)
                {
                    m_count--;
                    return true;
                }
                else
                {
                    return false;
                }
            }
       };
   }

   Semaphore::Semaphore(unsigned initial)
   {
       m_impl = new detail::SemaphoreImpl(initial);
   }

   Semaphore::~Semaphore()
   {
       delete m_impl;
   }

   void Semaphore::post()
   {
       m_impl->post();
   }

   void Semaphore::wait()
   {
       m_impl->wait();
   }

   bool Semaphore::tryWait()
   {
       return m_impl->tryWait();
   }

} }

#endif // !METHCLA_USE_CV_SEMAPHORE
