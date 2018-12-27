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

#ifndef METHCLA_MESSAGE_QUEUE_INTERFACE_HPP_INCLUDED
#define METHCLA_MESSAGE_QUEUE_INTERFACE_HPP_INCLUDED

namespace Methcla { namespace Utility {
    template <typename Message> class MessageQueueInterface
    {
    public:
        virtual ~MessageQueueInterface() {}
        virtual void send(const Message& msg) = 0;
        virtual bool next(Message& msg) = 0;
    };
}} // namespace Methcla::Utility

#endif // METHCLA_MESSAGE_QUEUE_INTERFACE_HPP_INCLUDED
