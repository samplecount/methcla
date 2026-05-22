// Copyright John Maddock 2008.
// Copyright Matt Borland 2024

// Use, modification and distribution are subject to the
// Boost Software License, Version 1.0.
// (See accompanying file LICENSE_1_0.txt
// or copy at http://www.boost.org/LICENSE_1_0.txt)

#ifndef BOOST_MATH_SPECIAL_ROUND_FWD_HPP
#define BOOST_MATH_SPECIAL_ROUND_FWD_HPP

#include <boost/math/tools/config.hpp>
#include <boost/math/tools/promotion.hpp>

#ifdef _MSC_VER
#pragma once
#endif

namespace methcla_boost
{
   namespace math
   { 

   template <class T, class Policy>
   BOOST_MATH_GPU_ENABLED typename tools::promote_args<T>::type trunc(const T& v, const Policy& pol);
   template <class T>
   BOOST_MATH_GPU_ENABLED typename tools::promote_args<T>::type trunc(const T& v);
   template <class T, class Policy>
   BOOST_MATH_GPU_ENABLED int itrunc(const T& v, const Policy& pol);
   template <class T>
   BOOST_MATH_GPU_ENABLED int itrunc(const T& v);
   template <class T, class Policy>
   BOOST_MATH_GPU_ENABLED long ltrunc(const T& v, const Policy& pol);
   template <class T>
   BOOST_MATH_GPU_ENABLED long ltrunc(const T& v);
   template <class T, class Policy>
   BOOST_MATH_GPU_ENABLED long long lltrunc(const T& v, const Policy& pol);
   template <class T>
   BOOST_MATH_GPU_ENABLED long long lltrunc(const T& v);
   template <class T, class Policy>
   BOOST_MATH_GPU_ENABLED typename tools::promote_args<T>::type round(const T& v, const Policy& pol);
   template <class T>
   BOOST_MATH_GPU_ENABLED typename tools::promote_args<T>::type round(const T& v);
   template <class T, class Policy>
   BOOST_MATH_GPU_ENABLED int iround(const T& v, const Policy& pol);
   template <class T>
   BOOST_MATH_GPU_ENABLED int iround(const T& v);
   template <class T, class Policy>
   BOOST_MATH_GPU_ENABLED long lround(const T& v, const Policy& pol);
   template <class T>
   BOOST_MATH_GPU_ENABLED long lround(const T& v);
   template <class T, class Policy>
   BOOST_MATH_GPU_ENABLED long long llround(const T& v, const Policy& pol);
   template <class T>
   BOOST_MATH_GPU_ENABLED long long llround(const T& v);
   template <class T, class Policy>
   BOOST_MATH_GPU_ENABLED T modf(const T& v, T* ipart, const Policy& pol);
   template <class T>
   BOOST_MATH_GPU_ENABLED T modf(const T& v, T* ipart);
   template <class T, class Policy>
   BOOST_MATH_GPU_ENABLED T modf(const T& v, int* ipart, const Policy& pol);
   template <class T>
   BOOST_MATH_GPU_ENABLED T modf(const T& v, int* ipart);
   template <class T, class Policy>
   BOOST_MATH_GPU_ENABLED T modf(const T& v, long* ipart, const Policy& pol);
   template <class T>
   BOOST_MATH_GPU_ENABLED T modf(const T& v, long* ipart);
   template <class T, class Policy>
   BOOST_MATH_GPU_ENABLED T modf(const T& v, long long* ipart, const Policy& pol);
   template <class T>
   BOOST_MATH_GPU_ENABLED T modf(const T& v, long long* ipart);
   }
}

#undef BOOST_MATH_STD_USING
#define BOOST_MATH_STD_USING BOOST_MATH_STD_USING_CORE\
   using methcla_boost::math::round;\
   using methcla_boost::math::iround;\
   using methcla_boost::math::lround;\
   using methcla_boost::math::trunc;\
   using methcla_boost::math::itrunc;\
   using methcla_boost::math::ltrunc;\
   using methcla_boost::math::modf;


#endif // BOOST_MATH_SPECIAL_ROUND_FWD_HPP

