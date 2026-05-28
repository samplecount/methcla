// Copyright (C) 2012-2013 Samplecount S.L.
// Copyright (C) 2026 Methcla contributors
//
// SPDX-License-Identifier: Apache-2.0

#pragma once

namespace Methcla { namespace Utility {
    template <typename Message> class MessageQueueInterface
    {
    public:
        virtual ~MessageQueueInterface()
        {}
        virtual void send(const Message& msg) = 0;
        virtual bool next(Message& msg) = 0;
    };
}} // namespace Methcla::Utility
