//-----------------------------------------------------------------------------
// boost variant/detail/element_index.hpp header file
// See http://www.boost.org for updates, documentation, and revision history.
//-----------------------------------------------------------------------------
//
// Copyright (c) 2014-2026 Antony Polukhin
//
// Distributed under the Boost Software License, Version 1.0. (See
// accompanying file LICENSE_1_0.txt or copy at
// http://www.boost.org/LICENSE_1_0.txt)

#ifndef BOOST_VARIANT_DETAIL_ELEMENT_INDEX_HPP
#define BOOST_VARIANT_DETAIL_ELEMENT_INDEX_HPP

#include <boost/config.hpp>
#include <boost/variant/recursive_wrapper_fwd.hpp>
#include <boost/variant/variant_fwd.hpp>

#include <boost/type_traits/remove_cv.hpp>
#include <boost/type_traits/remove_reference.hpp>
#include <boost/mpl/find_if.hpp>

namespace methcla_boost { namespace detail { namespace variant {

template <class VariantElement, class T>
struct variant_element_functor :
    methcla_boost::mpl::or_<
        methcla_boost::is_same<VariantElement, T>,
        methcla_boost::is_same<VariantElement, methcla_boost::recursive_wrapper<T> >,
        methcla_boost::is_same<VariantElement, T& >
    >
{};

template <class Types, class T>
struct element_iterator_impl :
    methcla_boost::mpl::find_if<
        Types,
        methcla_boost::mpl::or_<
            variant_element_functor<methcla_boost::mpl::_1, T>,
            variant_element_functor<methcla_boost::mpl::_1, typename methcla_boost::remove_cv<T>::type >
        >
    >
{};

template <class Variant, class T>
struct element_iterator :
    element_iterator_impl< typename Variant::types, typename methcla_boost::remove_reference<T>::type >
{};

template <class Variant, class T>
struct holds_element :
    methcla_boost::mpl::not_<
        methcla_boost::is_same<
            typename methcla_boost::mpl::end<typename Variant::types>::type,
            typename element_iterator<Variant, T>::type
        >
    >
{};


}}} // namespace methcla_boost::detail::variant

#endif // BOOST_VARIANT_DETAIL_ELEMENT_INDEX_HPP
