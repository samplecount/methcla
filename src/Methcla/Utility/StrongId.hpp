// Copyright (C) 2026 Methcla authors (https://github.com/samplecount/methcla)
//
// SPDX-License-Identifier: Apache-2.0

#pragma once

#include <ostream>

namespace Methcla::Utility {

    // Distinct named integer type. T is the underlying integer; Tag is a unique
    // empty struct that makes each instantiation a separate type. Use .id() to
    // access the underlying value.
    template <typename T, typename Tag> struct StrongId
    {
        explicit StrongId(T t) noexcept
        : m_id(t)
        {}
        StrongId() noexcept
        : m_id()
        {}
        T id() const noexcept
        {
            return m_id;
        }
        bool operator==(const StrongId& rhs) const noexcept
        {
            return m_id == rhs.m_id;
        }
        bool operator!=(const StrongId& rhs) const noexcept
        {
            return m_id != rhs.m_id;
        }
        bool operator<(const StrongId& rhs) const noexcept
        {
            return m_id < rhs.m_id;
        }
        bool operator<=(const StrongId& rhs) const noexcept
        {
            return m_id <= rhs.m_id;
        }
        bool operator>(const StrongId& rhs) const noexcept
        {
            return m_id > rhs.m_id;
        }
        bool operator>=(const StrongId& rhs) const noexcept
        {
            return m_id >= rhs.m_id;
        }

        friend std::ostream& operator<<(std::ostream& os, const StrongId& x)
        {
            return os << x.m_id;
        }

    private:
        T m_id;
    };

} // namespace Methcla::Utility
