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
#include <sstream>
#include <vector>
#include <thread>

#if defined(__native_client__)
# include <chrono>
#else
# include <time.h>
#endif

#include "gtest/gtest.h"

// Code from http://stackoverflow.com/a/29155677
namespace testing
{
    namespace internal
    {
        enum GTestColor
        {
            COLOR_DEFAULT,
            COLOR_RED,
            COLOR_GREEN,
            COLOR_YELLOW
        };

        extern void ColoredPrintf(GTestColor color, const char* fmt, ...);
    }
}

#define PRINTF(...) \
    do { \
        testing::internal::ColoredPrintf(testing::internal::COLOR_GREEN, "[          ] "); \
        testing::internal::ColoredPrintf(testing::internal::COLOR_YELLOW, __VA_ARGS__); \
    } while(0)

#define TEST_COUT Methcla::Tests::TestCout()

#define TEST_DESCRIPTION(desc) RecordProperty("description", desc)

namespace Methcla { namespace Tests {
    class TestCout : public std::stringstream
    {
    public:
        ~TestCout()
        {
            PRINTF("%s", str().c_str());
        }
    };

    void initialize(std::string inputFileDirectory, std::string outputFileDirectory);

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

    template <typename T> double rmsError(const T* a, const T* b, size_t n)
    {
        if (n == 0)
            return 0;

        double rms = 0.;

        for (size_t i=0; i < n; i++)
        {
            double x = a[i] - b[i];
            rms += x * x;
        }

        return sqrt(rms/(double)n);
    }

    template <typename T> double rmsError(const std::vector<T>& a, const std::vector<T>& b)
    {
        return rmsError<T>(a.data(), b.data(), std::min(a.size(), b.size()));
    }

    inline static void sleepFor(double seconds)
    {
#if defined(__native_client__)
        struct timespec ts;
        ts.tv_sec = seconds;
        ts.tv_nsec = (seconds - ts.tv_sec) * 1e9;
        nanosleep(&ts, &ts);
#else
        std::this_thread::sleep_for(std::chrono::duration<double>(seconds));
#endif
    }
} }

#endif // METHCLA_TESTS_HPP_INCLUDED