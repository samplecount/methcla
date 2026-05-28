// Copyright (C) 2012-2013 Samplecount S.L.
// Copyright (C) 2026 Methcla contributors
//
// SPDX-License-Identifier: Apache-2.0

#include "Methcla/Exception.hpp"
#include "Methcla/Utility/Semaphore.hpp"

#include <memory>

#if defined(__APPLE__)

#    include <mach/mach.h>
#    include <mach/mach_error.h>

namespace Methcla { namespace Utility { namespace detail {

    class SemaphoreImpl
    {
    public:
        SemaphoreImpl(unsigned initial)
        {
            const kern_return_t kr =
                semaphore_create(mach_task_self(), &m_sem, SYNC_POLICY_FIFO,
                                 static_cast<int>(initial));
            if (kr != KERN_SUCCESS)
                throw Error(kMethcla_SystemError, mach_error_string(kr));
        }

        ~SemaphoreImpl()
        {
            semaphore_destroy(mach_task_self(), m_sem);
        }

        void post()
        {
            semaphore_signal(m_sem);
        }

        void wait()
        {
            const kern_return_t kr = semaphore_wait(m_sem);
            if (kr != KERN_SUCCESS)
                throw Error(kMethcla_SystemError, mach_error_string(kr));
        }

        bool tryWait()
        {
            const mach_timespec_t zero = {0, 0};
            return semaphore_timedwait(m_sem, zero) == KERN_SUCCESS;
        }

    private:
        semaphore_t m_sem;
    };

}}} // namespace Methcla::Utility::detail

#elif defined(_WIN32)

#    include <string>

#    include <windows.h>

namespace {
    static std::string win32ErrorMessage(const DWORD code)
    {
        char* raw = nullptr;
        FormatMessageA(
            FORMAT_MESSAGE_ALLOCATE_BUFFER | FORMAT_MESSAGE_FROM_SYSTEM |
                FORMAT_MESSAGE_IGNORE_INSERTS,
            nullptr, code, 0, reinterpret_cast<LPSTR>(&raw), 0, nullptr);
        const auto deleter = [](char* p) { LocalFree(p); };
        const auto msg = std::unique_ptr<char, decltype(deleter)>(raw, deleter);
        return msg ? msg.get() : "unknown error";
    }
} // namespace

namespace Methcla { namespace Utility { namespace detail {

    class SemaphoreImpl
    {
    public:
        SemaphoreImpl(unsigned initial)
        {
            m_sem = CreateSemaphore(nullptr, static_cast<LONG>(initial),
                                    LONG_MAX, nullptr);
            if (m_sem == nullptr)
                throw Error(kMethcla_SystemError,
                            win32ErrorMessage(GetLastError()));
        }

        ~SemaphoreImpl()
        {
            CloseHandle(m_sem);
        }

        void post()
        {
            ReleaseSemaphore(m_sem, 1, nullptr);
        }

        void wait()
        {
            if (WaitForSingleObject(m_sem, INFINITE) != WAIT_OBJECT_0)
                throw Error(kMethcla_SystemError,
                            win32ErrorMessage(GetLastError()));
        }

        bool tryWait()
        {
            return WaitForSingleObject(m_sem, 0) == WAIT_OBJECT_0;
        }

    private:
        HANDLE m_sem;
    };

}}} // namespace Methcla::Utility::detail

#else // POSIX

#    include <cerrno>
#    include <cstring>

#    include <semaphore.h>

namespace Methcla { namespace Utility { namespace detail {

    class SemaphoreImpl
    {
    public:
        SemaphoreImpl(unsigned initial)
        {
            if (sem_init(&m_sem, 0, initial) != 0)
            {
                const int e = errno;
                throw Error(kMethcla_SystemError, strerror(e));
            }
        }

        ~SemaphoreImpl()
        {
            sem_destroy(&m_sem);
        }

        void post()
        {
            sem_post(&m_sem);
        }

        void wait()
        {
            while (sem_wait(&m_sem) != 0)
            {
                const int e = errno;
                if (e != EINTR)
                    throw Error(kMethcla_SystemError, strerror(e));
            }
        }

        bool tryWait()
        {
            return sem_trywait(&m_sem) == 0;
        }

    private:
        sem_t m_sem;
    };

}}} // namespace Methcla::Utility::detail

#endif

using namespace Methcla::Utility;

Semaphore::Semaphore(unsigned initial)
: m_impl(std::make_unique<detail::SemaphoreImpl>(initial))
{}

Semaphore::~Semaphore() = default;

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
