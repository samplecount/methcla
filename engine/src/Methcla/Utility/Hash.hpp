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
	struct string_equal_to
	{
		bool operator()(const char* const& s1, const char* const& s2) const
		{
			return strcmp(s1, s2) == 0;
		}
	};

	struct string_hash
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

// Copyright 2008-2009 Daniel James.
// Distributed under the Boost Software License, Version 1.0. (See accompanying
// file LICENSE_1_0.txt or copy at http://www.boost.org/LICENSE_1_0.txt)

// This code is also released into the public domain.

// Algorithm from: http://www.isthe.com/chongo/tech/comp/fnv/

    // template <std::size_t FnvPrime, std::size_t OffsetBasis>
    // 
    // struct basic_fnv_1
    // {
    //     std::size_t operator()(std::string const& text) const
    //     {
    //         std::size_t hash = OffsetBasis;
    //         for(std::string::const_iterator it = text.begin(), end = text.end();
    //                 it != end; ++it)
    //         {
    //             hash *= FnvPrime;
    //             hash ^= *it;
    //         }
    // 
    //         return hash;
    //     }
    // };
    // 
    // template <std::size_t FnvPrime, std::size_t OffsetBasis>
    // struct basic_fnv_1a
    // {
    //     std::size_t operator()(std::string const& text) const
    //     {
    //         std::size_t hash = OffsetBasis;
    //         for(std::string::const_iterator it = text.begin(), end = text.end();
    //                 it != end; ++it)
    //         {
    //             hash ^= *it;
    //             hash *= FnvPrime;
    //         }
    // 
    //         return hash;
    //     }
    // };
    // 
    // // For 32 bit machines:
    // const std::size_t fnv_prime = 16777619u;
    // const std::size_t fnv_offset_basis = 2166136261u;
    // 
    // // For 64 bit machines:
    // // const std::size_t fnv_prime = 1099511628211u;
    // // const std::size_t fnv_offset_basis = 14695981039346656037u;
    // 
    // // For 128 bit machines:
    // // const std::size_t fnv_prime = 309485009821345068724781401u;
    // // const std::size_t fnv_offset_basis =
    // //     275519064689413815358837431229664493455u;
    // 
    // // For 256 bit machines:
    // // const std::size_t fnv_prime =
    // //     374144419156711147060143317175368453031918731002211u;
    // // const std::size_t fnv_offset_basis =
    // //     100029257958052580907070968620625704837092796014241193945225284501741471925557u;
    // 
    // typedef basic_fnv_1<fnv_prime, fnv_offset_basis> fnv_1;
    // typedef basic_fnv_1a<fnv_prime, fnv_offset_basis> fnv_1a;

} } }

#endif // METHCLA_UTILITY_HASH_HPP_INCLUDED