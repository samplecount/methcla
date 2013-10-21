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
public:
    Error(Methcla_Error code, const std::string& description="")
        : m_code(code)
        , m_description(description)
    { }

    Methcla_Error errorCode() const noexcept
    {
        return m_code;
    }

    const char* what() const noexcept override
    {
        return m_description.empty() ? methcla_error_message(m_code) : m_description.c_str();
    }

    static Error unspecified() { return Error(kMethcla_UnspecifiedError); }
    static Error memory() { return Error(kMethcla_MemoryError); }

private:
    Methcla_Error   m_code;
    std::string     m_description;
};
}

#endif // METHCLA_EXCEPTION_HPP_INCLUDED
