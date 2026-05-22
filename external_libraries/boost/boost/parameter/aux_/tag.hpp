// Copyright David Abrahams 2005.
// Copyright Cromwell D. Enage 2017.
// Distributed under the Boost Software License, Version 1.0.
// (See accompanying file LICENSE_1_0.txt or copy at
// http://www.boost.org/LICENSE_1_0.txt)

#ifndef BOOST_PARAMETER_AUX_TAG_DWA2005610_HPP
#define BOOST_PARAMETER_AUX_TAG_DWA2005610_HPP

#include <boost/parameter/aux_/unwrap_cv_reference.hpp>
#include <boost/parameter/aux_/tagged_argument.hpp>
#include <boost/parameter/config.hpp>

#if defined(BOOST_PARAMETER_CAN_USE_MP11) && \
    !BOOST_WORKAROUND(BOOST_MSVC, >= 1910)
// MSVC-14.1+ assigns rvalue references to tagged_argument instances
// instead of tagged_argument_rref instances with this code.
#include <boost/mp11/integral.hpp>
#include <boost/mp11/utility.hpp>
#include <type_traits>

namespace methcla_boost { namespace parameter { namespace aux {

    template <typename Keyword, typename Arg>
    struct tag_if_lvalue_reference
    {
        using type = ::methcla_boost::parameter::aux::tagged_argument_list_of_1<
            ::methcla_boost::parameter::aux::tagged_argument<
                Keyword
              , typename ::methcla_boost::parameter::aux
                ::unwrap_cv_reference<Arg>::type
            >
        >;
    };

    template <typename Keyword, typename Arg>
    struct tag_if_scalar
    {
        using type = ::methcla_boost::parameter::aux::tagged_argument_list_of_1<
            ::methcla_boost::parameter::aux
            ::tagged_argument<Keyword,typename ::std::add_const<Arg>::type>
        >;
    };

    template <typename Keyword, typename Arg>
    using tag_if_otherwise = ::methcla_boost::mp11::mp_if<
        ::std::is_scalar<typename ::std::remove_const<Arg>::type>
      , ::methcla_boost::parameter::aux::tag_if_scalar<Keyword,Arg>
      , ::methcla_boost::mp11::mp_identity<
            ::methcla_boost::parameter::aux::tagged_argument_list_of_1<
                ::methcla_boost::parameter::aux::tagged_argument_rref<Keyword,Arg>
            >
        >
    >;

    template <typename Keyword, typename Arg>
    using tag = ::methcla_boost::mp11::mp_if<
        ::methcla_boost::mp11::mp_if<
            ::std::is_lvalue_reference<Arg>
          , ::methcla_boost::mp11::mp_true
          , ::methcla_boost::parameter::aux::is_cv_reference_wrapper<Arg>
        >
      , ::methcla_boost::parameter::aux::tag_if_lvalue_reference<Keyword,Arg>
      , ::methcla_boost::parameter::aux::tag_if_otherwise<Keyword,Arg>
    >;
}}} // namespace methcla_boost::parameter::aux_

#elif defined(BOOST_PARAMETER_HAS_PERFECT_FORWARDING)
#include <boost/mpl/bool.hpp>
#include <boost/mpl/if.hpp>
#include <boost/mpl/eval_if.hpp>
#include <boost/mpl/identity.hpp>
#include <boost/type_traits/add_const.hpp>
#include <boost/type_traits/is_scalar.hpp>
#include <boost/type_traits/is_lvalue_reference.hpp>
#include <boost/type_traits/remove_const.hpp>

namespace methcla_boost { namespace parameter { namespace aux {

    template <typename Keyword, typename ActualArg>
    struct tag
    {
        typedef typename ::methcla_boost::parameter::aux
        ::unwrap_cv_reference<ActualArg>::type Arg;
        typedef typename ::methcla_boost::add_const<Arg>::type ConstArg;
        typedef typename ::methcla_boost::remove_const<Arg>::type MutArg;
        typedef typename ::methcla_boost::mpl::eval_if<
            typename ::methcla_boost::mpl::if_<
                ::methcla_boost::is_lvalue_reference<ActualArg>
              , ::methcla_boost::mpl::true_
              , ::methcla_boost::parameter::aux::is_cv_reference_wrapper<ActualArg>
            >::type
          , ::methcla_boost::mpl::identity<
#if defined(BOOST_PARAMETER_CAN_USE_MP11)
                ::methcla_boost::parameter::aux::tagged_argument_list_of_1<
#endif
                    ::methcla_boost::parameter::aux::tagged_argument<Keyword,Arg>
#if defined(BOOST_PARAMETER_CAN_USE_MP11)
                >
#endif
            >
          , ::methcla_boost::mpl::if_<
                ::methcla_boost::is_scalar<MutArg>
#if defined(BOOST_PARAMETER_CAN_USE_MP11)
              , ::methcla_boost::parameter::aux::tagged_argument_list_of_1<
                    ::methcla_boost::parameter::aux::tagged_argument<Keyword,ConstArg>
                >
              , ::methcla_boost::parameter::aux::tagged_argument_list_of_1<
                    ::methcla_boost::parameter::aux::tagged_argument_rref<Keyword,Arg>
                >
#else
              , ::methcla_boost::parameter::aux::tagged_argument<Keyword,ConstArg>
              , ::methcla_boost::parameter::aux::tagged_argument_rref<Keyword,Arg>
#endif
            >
        >::type type;
    };
}}} // namespace methcla_boost::parameter::aux_

#else   // !defined(BOOST_PARAMETER_HAS_PERFECT_FORWARDING)

namespace methcla_boost { namespace parameter { namespace aux {

    template <
        typename Keyword
      , typename Arg
#if BOOST_WORKAROUND(BOOST_BORLANDC, BOOST_TESTED_AT(0x564))
      , typename = typename ::methcla_boost::parameter::aux
        ::is_cv_reference_wrapper<Arg>::type
#endif
    >
    struct tag
    {
        typedef ::methcla_boost::parameter::aux::tagged_argument<
            Keyword
          , typename ::methcla_boost::parameter::aux::unwrap_cv_reference<Arg>::type
        > type;
    };
}}} // namespace methcla_boost::parameter::aux_

#if BOOST_WORKAROUND(BOOST_BORLANDC, BOOST_TESTED_AT(0x564))
#include <boost/mpl/bool.hpp>
#include <boost/type_traits/remove_reference.hpp>

namespace methcla_boost { namespace parameter { namespace aux {

    template <typename Keyword, typename Arg>
    struct tag<Keyword,Arg,::methcla_boost::mpl::false_>
    {
        typedef ::methcla_boost::parameter::aux::tagged_argument<
            Keyword
          , typename ::methcla_boost::remove_reference<Arg>::type
        > type;
    };
}}} // namespace methcla_boost::parameter::aux_

#endif  // Borland workarounds needed.
#endif  // MP11 or perfect forwarding support
#endif  // include guard

