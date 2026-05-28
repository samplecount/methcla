// Copyright (C) 2012-2013 Samplecount S.L.
// Copyright (C) 2026 Methcla contributors
//
// SPDX-License-Identifier: Apache-2.0

#pragma once

namespace Methcla { namespace Utility {

    namespace detail {
        class SemaphoreImpl;
    }

    class Semaphore
    {
    public:
        Semaphore(unsigned initial = 0);
        ~Semaphore();

        Semaphore(const Semaphore&) = delete;
        Semaphore& operator=(const Semaphore&) = delete;

        void post();
        void wait();
        bool tryWait();

    private:
        detail::SemaphoreImpl* m_impl;
    };

}} // namespace Methcla::Utility
