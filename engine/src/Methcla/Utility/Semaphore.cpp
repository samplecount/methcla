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
#include "zix/sem.h"

#include <memory>
#include <stdexcept>

using namespace Methcla::Utility;

struct Methcla::Utility::detail::SemaphoreImpl : public ZixSem { };

static void check(ZixStatus status)
{
    switch (status) {
        case ZIX_STATUS_SUCCESS:
            break;
        case ZIX_STATUS_ERROR:
            throw std::runtime_error("Semaphore: Unknown error");
        case ZIX_STATUS_NO_MEM:
            throw std::bad_alloc();
        case ZIX_STATUS_NOT_FOUND:
            throw std::runtime_error("Semaphore: Not found");
        case ZIX_STATUS_EXISTS:
            throw std::runtime_error("Semaphore: Exists");
        case ZIX_STATUS_BAD_ARG:
            throw std::invalid_argument("Semaphore");
        case ZIX_STATUS_BAD_PERMS:
            throw std::runtime_error("Semaphore: Access denied");
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
