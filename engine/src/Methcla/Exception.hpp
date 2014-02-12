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

#ifndef METHCLA_EXCEPTION_HPP_INCLUDED
#define METHCLA_EXCEPTION_HPP_INCLUDED

#include <methcla/common.h>
#include <stdexcept>
#include <string>

namespace Methcla {

class Exception : public virtual std::exception
{
};

class Error : public Exception
{
    Methcla_ErrorCode   m_code;
    std::string         m_message;

public:
    Error(Methcla_ErrorCode code, const std::string& message="")
        : m_code(code)
        , m_message(message)
    {}

    Methcla_ErrorCode errorCode() const noexcept
    {
        return m_code;
    }

    const char* errorMessage() const noexcept
    {
        return m_message.empty()
                ? methcla_error_code_description(m_code)
                : m_message.c_str();
    }

    const char* what() const noexcept override
    {
        return errorMessage();
    }

    operator Methcla_Error() const
    {
        return methcla_error_new_with_message(
            errorCode(),
            m_message.empty() ? nullptr : m_message.c_str()
        );
    }

    static Error unspecified() { return Error(kMethcla_UnspecifiedError); }
    static Error memory() { return Error(kMethcla_MemoryError); }
};
}

#endif // METHCLA_EXCEPTION_HPP_INCLUDED
