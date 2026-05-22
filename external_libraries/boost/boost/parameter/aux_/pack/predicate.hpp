// Copyright David Abrahams, Daniel Wallin 2003.
// Distributed under the Boost Software License, Version 1.0.
// (See accompanying file LICENSE_1_0.txt or copy at
// http://www.boost.org/LICENSE_1_0.txt)

#ifndef BOOST_PARAMETER_AUX_PACK_PREDICATE_HPP
#define BOOST_PARAMETER_AUX_PACK_PREDICATE_HPP

namespace methcla_boost { namespace parameter { namespace aux {

    // helper for get_predicate<...>, below
    template <typename T>
    struct get_predicate_or_default
    {
        typedef T type;
    };

    // helper for predicate<...>, below
    template <typename T>
    struct get_predicate
      : ::methcla_boost::parameter::aux
        ::get_predicate_or_default<typename T::predicate>
    {
    };
}}} // namespace methcla_boost::parameter::aux

#include <boost/parameter/aux_/use_default.hpp>
#include <boost/parameter/aux_/always_true_predicate.hpp>

namespace methcla_boost { namespace parameter { namespace aux {

    template <>
    struct get_predicate_or_default< ::methcla_boost::parameter::aux::use_default>
    {
        typedef ::methcla_boost::parameter::aux::always_true_predicate type;
    };
}}} // namespace methcla_boost::parameter::aux

#include <boost/parameter/required.hpp>
#include <boost/parameter/optional.hpp>
#include <boost/parameter/config.hpp>

#if defined(BOOST_PARAMETER_CAN_USE_MP11)
#include <boost/mp11/integral.hpp>
#include <boost/mp11/utility.hpp>
#else
#include <boost/mpl/bool.hpp>
#include <boost/mpl/if.hpp>
#include <boost/mpl/eval_if.hpp>
#include <boost/mpl/identity.hpp>
#endif

namespace methcla_boost { namespace parameter { namespace aux {

    template <typename T>
#if defined(BOOST_PARAMETER_CAN_USE_MP11)
    using predicate = ::methcla_boost::mp11::mp_if<
        ::methcla_boost::mp11::mp_if<
            ::methcla_boost::parameter::aux::is_optional<T>
          , ::methcla_boost::mp11::mp_true
          , ::methcla_boost::parameter::aux::is_required<T>
        >
      , ::methcla_boost::parameter::aux::get_predicate<T>
      , ::methcla_boost::mp11::mp_identity<
            ::methcla_boost::parameter::aux::always_true_predicate
        >
    >;
#else
    struct predicate
      : ::methcla_boost::mpl::eval_if<
            typename ::methcla_boost::mpl::if_<
                ::methcla_boost::parameter::aux::is_optional<T>
              , ::methcla_boost::mpl::true_
              , ::methcla_boost::parameter::aux::is_required<T>
            >::type
          , ::methcla_boost::parameter::aux::get_predicate<T>
          , ::methcla_boost::mpl::identity<
                ::methcla_boost::parameter::aux::always_true_predicate
            >
        >
    {
    };
#endif  // BOOST_PARAMETER_CAN_USE_MP11
}}} // namespace methcla_boost::parameter::aux

#endif  // include guard

