// Copyright (C) 2003, 2008 Fernando Luis Cacciola Carballal.
// Copyright (C) 2015 Andrzej Krzemienski.
//
// Use, modification, and distribution is subject to the Boost Software
// License, Version 1.0. (See accompanying file LICENSE_1_0.txt or copy at
// http://www.boost.org/LICENSE_1_0.txt)
//
// See http://www.boost.org/libs/optional for documentation.
//
// You are welcome to contact the author at:
//  akrzemi1@gmail.com

#ifndef BOOST_OPTIONAL_DETAIL_OPTIONAL_SWAP_AJK_28JAN2015_HPP
#define BOOST_OPTIONAL_DETAIL_OPTIONAL_SWAP_AJK_28JAN2015_HPP

#include <boost/core/invoke_swap.hpp>
#include <boost/optional/optional_fwd.hpp>

namespace methcla_boost {

namespace optional_detail {

template <bool use_default_constructor> struct swap_selector;

template <>
struct swap_selector<true>
{
    template <class T>
    static void optional_swap ( optional<T>& x, optional<T>& y )
    {
        const bool hasX = !!x;
        const bool hasY = !!y;

        if ( !hasX && !hasY )
            return;

        if( !hasX )
            x.emplace();
        else if ( !hasY )
            y.emplace();

        // Boost.Core.Swap will take care of ADL and workarounds for broken compilers
        methcla_boost::core::invoke_swap(x.get(), y.get());

        if( !hasX )
            y = methcla_boost::none ;
        else if( !hasY )
            x = methcla_boost::none ;
    }
};

#ifdef BOOST_OPTIONAL_DETAIL_MOVE
# undef BOOST_OPTIONAL_DETAIL_MOVE
#endif

#ifndef BOOST_OPTIONAL_DETAIL_NO_RVALUE_REFERENCES
# define BOOST_OPTIONAL_DETAIL_MOVE(EXPR_) optional_detail::move_(EXPR_)
#else
# define BOOST_OPTIONAL_DETAIL_MOVE(EXPR_) EXPR_
#endif

template <>
struct swap_selector<false>
{
    template <class T>
    static void optional_swap ( optional<T>& x, optional<T>& y )
    //BOOST_NOEXCEPT_IF(::methcla_boost::is_nothrow_move_constructible<T>::value && BOOST_NOEXCEPT_EXPR(methcla_boost::core::invoke_swap(*x, *y)))
    {
        if (x)
        {
            if (y)
            {
                methcla_boost::core::invoke_swap(*x, *y);
            }
            else
            {
                y = BOOST_OPTIONAL_DETAIL_MOVE(*x);
                x = methcla_boost::none;
            }
        }
        else
        {
            if (y)
            {
                x = BOOST_OPTIONAL_DETAIL_MOVE(*y);
                y = methcla_boost::none;
            }
        }
    }
};

} // namespace optional_detail

#if (!defined BOOST_CONFIG_RESTORE_OBSOLETE_SWAP_IMPLEMENTATION)

template<class T>
struct optional_swap_should_use_default_constructor : methcla_boost::false_type {} ;

#else

template<class T>
struct optional_swap_should_use_default_constructor : has_nothrow_default_constructor<T> {} ;

#endif

template <class T>
inline void swap ( optional<T>& x, optional<T>& y )
//BOOST_NOEXCEPT_IF(::methcla_boost::is_nothrow_move_constructible<T>::value && BOOST_NOEXCEPT_EXPR(methcla_boost::core::invoke_swap(*x, *y)))
{
    optional_detail::swap_selector<optional_swap_should_use_default_constructor<T>::value>::optional_swap(x, y);
}

} // namespace methcla_boost

#undef BOOST_OPTIONAL_DETAIL_MOVE

#endif // header guard
