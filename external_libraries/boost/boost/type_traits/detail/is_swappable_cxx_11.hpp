#ifndef BOOST_TYPE_TRAITS_DETAIL_IS_SWAPPABLE_CXX_11_HPP_INCLUDED
#define BOOST_TYPE_TRAITS_DETAIL_IS_SWAPPABLE_CXX_11_HPP_INCLUDED

//  Copyright 2017 Peter Dimov
//  Copyright 2023 Andrey Semashev
//
//  Distributed under the Boost Software License, Version 1.0.
//  See accompanying file LICENSE_1_0.txt or copy at
//  http://www.boost.org/LICENSE_1_0.txt

#include <boost/config.hpp>
#include <boost/config/workaround.hpp>
#include <boost/type_traits/declval.hpp>
#include <boost/type_traits/integral_constant.hpp>
#if __cplusplus >= 201103L || defined(BOOST_DINKUMWARE_STDLIB)
#include <utility> // for std::swap (C++11)
#else
#include <algorithm> // for std::swap (C++98)
#endif

// Intentionally not within boost namespace to avoid implicitly pulling in methcla_boost::swap overloads other than through ADL
namespace methcla_boost_type_traits_swappable_detail
{

using std::swap;

template<class T, class U, class = decltype(swap(methcla_boost::declval<T>(), methcla_boost::declval<U>()))> methcla_boost::true_type is_swappable_with_impl( int );
template<class T, class U> methcla_boost::false_type is_swappable_with_impl( ... );
template<class T, class U>
struct is_swappable_with_helper { typedef decltype( methcla_boost_type_traits_swappable_detail::is_swappable_with_impl<T, U>(0) ) type; };

template<class T, class = decltype(swap(methcla_boost::declval<T&>(), methcla_boost::declval<T&>()))> methcla_boost::true_type is_swappable_impl( int );
template<class T> methcla_boost::false_type is_swappable_impl( ... );
template<class T>
struct is_swappable_helper { typedef decltype( methcla_boost_type_traits_swappable_detail::is_swappable_impl<T>(0) ) type; };

#if !defined(BOOST_NO_CXX11_NOEXCEPT)

#if BOOST_WORKAROUND(BOOST_GCC, < 40700)

// gcc 4.6 ICEs when noexcept operator is used on an invalid expression
template<class T, class U, bool = is_swappable_with_helper<T, U>::type::value>
struct is_nothrow_swappable_with_helper { typedef methcla_boost::false_type type; };
template<class T, class U>
struct is_nothrow_swappable_with_helper<T, U, true> { typedef methcla_boost::integral_constant<bool, noexcept(swap(methcla_boost::declval<T>(), methcla_boost::declval<U>()))> type; };

template<class T, bool = is_swappable_helper<T>::type::value>
struct is_nothrow_swappable_helper { typedef methcla_boost::false_type type; };
template<class T>
struct is_nothrow_swappable_helper<T, true> { typedef methcla_boost::integral_constant<bool, noexcept(swap(methcla_boost::declval<T&>(), methcla_boost::declval<T&>()))> type; };

#else // BOOST_WORKAROUND(BOOST_GCC, < 40700)

template<class T, class U, bool B = noexcept(swap(methcla_boost::declval<T>(), methcla_boost::declval<U>()))> methcla_boost::integral_constant<bool, B> is_nothrow_swappable_with_impl( int );
template<class T, class U> methcla_boost::false_type is_nothrow_swappable_with_impl( ... );
template<class T, class U>
struct is_nothrow_swappable_with_helper { typedef decltype( methcla_boost_type_traits_swappable_detail::is_nothrow_swappable_with_impl<T, U>(0) ) type; };

template<class T, bool B = noexcept(swap(methcla_boost::declval<T&>(), methcla_boost::declval<T&>()))> methcla_boost::integral_constant<bool, B> is_nothrow_swappable_impl( int );
template<class T> methcla_boost::false_type is_nothrow_swappable_impl( ... );
template<class T>
struct is_nothrow_swappable_helper { typedef decltype( methcla_boost_type_traits_swappable_detail::is_nothrow_swappable_impl<T>(0) ) type; };

#endif // BOOST_WORKAROUND(BOOST_GCC, < 40700)

#endif // !defined(BOOST_NO_CXX11_NOEXCEPT)

} // namespace methcla_boost_type_traits_swappable_detail

#endif // #ifndef BOOST_TYPE_TRAITS_DETAIL_IS_SWAPPABLE_CXX_11_HPP_INCLUDED
