// Copyright 2012-2013 Samplecount S.L.
//
// SPDX-License-Identifier: Apache-2.0

#ifndef METHCLA_UTILITY_HASH_HPP_INCLUDED
#define METHCLA_UTILITY_HASH_HPP_INCLUDED

#include <boost/functional/hash.hpp>

#include <cstring>

namespace Methcla { namespace Utility { namespace Hash {
    struct cstr_equal
    {
        bool operator()(const char* const& s1, const char* const& s2) const
        {
            return strcmp(s1, s2) == 0;
        }
    };

    struct cstr_hash
    {
        size_t operator()(const char* const& str) const
        {
            size_t seed = 0;
            for (const char* it = str; *it != '\0'; it++)
            {
                methcla_boost::hash_combine(seed, *it);
            }
            return seed;
        }
    };
}}} // namespace Methcla::Utility::Hash

#endif // METHCLA_UTILITY_HASH_HPP_INCLUDED
