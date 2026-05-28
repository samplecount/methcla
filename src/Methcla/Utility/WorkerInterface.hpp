// Copyright (C) 2012-2013 Samplecount S.L.
// Copyright (C) 2026 Methcla contributors
//
// SPDX-License-Identifier: Apache-2.0

#ifndef METHCLA_WORKER_INTERFACE_HPP_INCLUDED
#define METHCLA_WORKER_INTERFACE_HPP_INCLUDED

namespace Methcla { namespace Utility {
    template <typename Command> class WorkerInterface
    {
    public:
        virtual ~WorkerInterface()
        {}

        virtual void stop() {};

        virtual void sendToWorker(const Command& cmd) = 0;
        virtual void sendFromWorker(const Command& cmd) = 0;

        virtual void perform() = 0;
    };
}} // namespace Methcla::Utility

#endif // METHCLA_WORKER_INTERFACE_HPP_INCLUDED
