// Copyright 2012-2013 Samplecount S.L.
//
// SPDX-License-Identifier: Apache-2.0

#ifndef METHCLA_EXCEPTION_HPP_INCLUDED
#define METHCLA_EXCEPTION_HPP_INCLUDED

#include <methcla/common.h>

#include <stdexcept>
#include <string>

namespace Methcla {

    class Exception : public virtual std::exception
    {};

    class Error : public Exception
    {
        Methcla_ErrorCode m_code;
        std::string       m_message;

    public:
        Error(Methcla_ErrorCode code, const std::string& message = "")
        : m_code(code)
        , m_message(message)
        {}

        Methcla_ErrorCode errorCode() const noexcept
        {
            return m_code;
        }

        const char* errorMessage() const noexcept
        {
            return m_message.empty() ? methcla_error_code_description(m_code)
                                     : m_message.c_str();
        }

        const char* what() const noexcept override
        {
            return errorMessage();
        }
    };
} // namespace Methcla

#endif // METHCLA_EXCEPTION_HPP_INCLUDED
