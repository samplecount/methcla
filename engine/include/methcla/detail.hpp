// Copyright 2013 Samplecount S.L.
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

#ifndef METHCLA_DETAIL_HPP_INCLUDED
#define METHCLA_DETAIL_HPP_INCLUDED

#include <stdexcept>
#include <memory>

namespace Methcla
{
    namespace detail
    {
        template <class D, typename T> class Id
        {
        public:
            explicit Id(T id)
                : m_id(id)
            { }
            Id(const D& other)
                : m_id(other.m_id)
            { }

            T id() const
            {
                return m_id;
            }

            bool operator==(const D& other) const
            {
                return m_id == other.m_id;
            }

            bool operator!=(const D& other) const
            {
                return m_id != other.m_id;
            }

        private:
            T m_id;
        };

        inline static void throwError(Methcla_Error err, const char* msg)
        {
            if (err != kMethcla_NoError)
            {
                if (err == kMethcla_ArgumentError) {
                    throw std::invalid_argument(msg);
                } else if (err == kMethcla_LogicError) {
                    throw std::logic_error(msg);
                } else if (err == kMethcla_MemoryError) {
                    throw std::bad_alloc();
                } else {
                    throw std::runtime_error(msg);
                }
            }
        }

        inline static void checkReturnCode(Methcla_Error err)
        {
            throwError(err, methcla_error_message(err));
        }

        template <typename T> T combineFlags(T a, T b)
        {
            typedef typename std::underlying_type<T>::type enum_type;
            return static_cast<T>(static_cast<enum_type>(a) | static_cast<enum_type>(b));
        }
    }
}

#endif // METHCLA_DETAIL_HPP_INCLUDED
