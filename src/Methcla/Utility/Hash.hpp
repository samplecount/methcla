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

#ifndef METHCLA_UTILITY_HASH_HPP_INCLUDED
#define METHCLA_UTILITY_HASH_HPP_INCLUDED

#include <boost/functional/hash.hpp>
#include <cstring>

namespace Methcla { namespace Utility { namespace Hash
{
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
            for (const char* it = str; *it != '\0'; it++) {
                boost::hash_combine(seed, *it);
            }
            return seed;
        }
    };
} } }

#endif // METHCLA_UTILITY_HASH_HPP_INCLUDED
