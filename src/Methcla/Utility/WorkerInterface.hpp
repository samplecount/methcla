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

#ifndef METHCLA_WORKER_INTERFACE_HPP_INCLUDED
#define METHCLA_WORKER_INTERFACE_HPP_INCLUDED

namespace Methcla { namespace Utility {
    template <typename Command> class WorkerInterface
    {
    public:
        virtual ~WorkerInterface() { }

        virtual void stop() { };

        virtual void sendToWorker(const Command& cmd) = 0;
        virtual void sendFromWorker(const Command& cmd) = 0;

        virtual void perform() = 0;
    };
} }

#endif // METHCLA_WORKER_INTERFACE_HPP_INCLUDED
