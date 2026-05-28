// Copyright 2012-2013 Samplecount S.L.
//
// SPDX-License-Identifier: Apache-2.0

#ifndef METHCLA_UTILITY_SEMAPHORE_HPP_INCLUDED
#define METHCLA_UTILITY_SEMAPHORE_HPP_INCLUDED

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

#endif // METHCLA_UTILITY_SEMAPHORE_HPP_INCLUDED
