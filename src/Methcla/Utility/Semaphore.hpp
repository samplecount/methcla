// Copyright (C) 2012-2013 Samplecount S.L.
// Copyright (C) 2026 Methcla contributors
//
// SPDX-License-Identifier: Apache-2.0

#pragma once

#include <memory>

namespace Methcla { namespace Utility {

    namespace detail {
        class SemaphoreImpl;
    }

    // Counting semaphore. post() is realtime-safe and may be called from the
    // audio thread. wait() and tryWait() must only be called from non-RT
    // threads.
    //
    // Platform backends: Mach (macOS/iOS), Win32, POSIX sem_t (Linux).
    class Semaphore
    {
    public:
        explicit Semaphore(unsigned initial = 0);
        ~Semaphore();

        Semaphore(const Semaphore&) = delete;
        Semaphore& operator=(const Semaphore&) = delete;

        // Increment the count and wake one waiter. Realtime-safe.
        void post();

        // Block until count > 0, then decrement. Not realtime-safe.
        void wait();

        // Decrement if count > 0, return true. Return false immediately if
        // zero.
        bool tryWait();

    private:
        std::unique_ptr<detail::SemaphoreImpl> m_impl;
    };

}} // namespace Methcla::Utility
