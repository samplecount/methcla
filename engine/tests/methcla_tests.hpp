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

#ifndef METHCLA_TESTS_HPP_INCLUDED
#define METHCLA_TESTS_HPP_INCLUDED

#include <cmath>
#include <limits>
#include <string>

namespace Methcla { namespace Tests {
    std::string inputFile(const std::string& name);
    std::string outputFile(const std::string& name);

    template <typename T> bool nearlyEqual(T a, T b, T epsilon=std::numeric_limits<T>::epsilon())
    {
        const T absA = std::fabs(a);
        const T absB = std::fabs(b);
        const T diff = std::fabs(a - b);

        if (a == b)
        {
            // Shortcut, handles infinities
            return true;
        }
        else if (a == 0 || b == 0 || diff < std::numeric_limits<T>::denorm_min())
        {
            // a or b is zero or both are extremely close to it
            // relative error is less meaningful here
            return diff < (epsilon * std::numeric_limits<T>::denorm_min());
        }
        else
        {
            // use relative error
            return diff / (absA + absB) < epsilon;
        }
    }
} }

#endif // METHCLA_TESTS_HPP_INCLUDED