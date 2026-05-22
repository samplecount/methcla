// Copyright David Abrahams 2005.
// Distributed under the Boost Software License, Version 1.0.
// (See accompanying file LICENSE_1_0.txt or copy at
// http://www.boost.org/LICENSE_1_0.txt)

#ifndef BOOST_PARAMETER_BINDING_DWA200558_HPP
#define BOOST_PARAMETER_BINDING_DWA200558_HPP

#include <boost/parameter/aux_/void.hpp>
#include <boost/parameter/config.hpp>

#if defined(BOOST_PARAMETER_CAN_USE_MP11)
#include <boost/mp11/integral.hpp>
#include <boost/mp11/list.hpp>
#include <boost/mp11/utility.hpp>
#include <type_traits>
#else
#include <boost/mpl/bool.hpp>
#include <boost/mpl/if.hpp>
#include <boost/mpl/eval_if.hpp>
#include <boost/mpl/identity.hpp>
#include <boost/mpl/apply_wrap.hpp>
#include <boost/mpl/assert.hpp>
#include <boost/type_traits/is_same.hpp>
#endif

namespace methcla_boost { namespace parameter {

    // A metafunction that, given an argument pack, returns the reference type
    // of the parameter identified by the given keyword.  If no such parameter
    // has been specified, returns Default

    template <typename Parameters, typename Keyword, typename Default>
    struct binding0
    {
#if defined(BOOST_PARAMETER_CAN_USE_MP11)
        using type = ::methcla_boost::mp11::mp_apply_q<
            typename Parameters::binding
          , ::methcla_boost::mp11::mp_list<Keyword,Default,::methcla_boost::mp11::mp_true>
        >;

        static_assert(
            ::methcla_boost::mp11::mp_if<
                ::std::is_same<Default,::methcla_boost::parameter::void_>
              , ::methcla_boost::mp11::mp_if<
                    ::std::is_same<type,::methcla_boost::parameter::void_>
                  , ::methcla_boost::mp11::mp_false
                  , ::methcla_boost::mp11::mp_true
                >
              , ::methcla_boost::mp11::mp_true
            >::value
          , "required parameters must not result in void_ type"
        );
#else   // !defined(BOOST_PARAMETER_CAN_USE_MP11)
        typedef typename ::methcla_boost::mpl::apply_wrap3<
            typename Parameters::binding
          , Keyword
          , Default
          , ::methcla_boost::mpl::true_
        >::type type;

        BOOST_MPL_ASSERT((
            typename ::methcla_boost::mpl::eval_if<
                ::methcla_boost::is_same<Default,::methcla_boost::parameter::void_>
              , ::methcla_boost::mpl::if_<
                    ::methcla_boost::is_same<type,::methcla_boost::parameter::void_>
                  , ::methcla_boost::mpl::false_
                  , ::methcla_boost::mpl::true_
                >
              , ::methcla_boost::mpl::true_
            >::type
        ));
#endif  // BOOST_PARAMETER_CAN_USE_MP11
    };

#if defined(BOOST_PARAMETER_CAN_USE_MP11)
    template <typename Placeholder, typename Keyword, typename Default>
    struct binding1
    {
        using type = ::methcla_boost::mp11::mp_apply_q<
            Placeholder
          , ::methcla_boost::mp11::mp_list<Keyword,Default,::methcla_boost::mp11::mp_true>
        >;

        static_assert(
            ::methcla_boost::mp11::mp_if<
                ::std::is_same<Default,::methcla_boost::parameter::void_>
              , ::methcla_boost::mp11::mp_if<
                    ::std::is_same<type,::methcla_boost::parameter::void_>
                  , ::methcla_boost::mp11::mp_false
                  , ::methcla_boost::mp11::mp_true
                >
              , ::methcla_boost::mp11::mp_true
            >::value
          , "required parameters must not result in void_ type"
        );
    };
#endif  // BOOST_PARAMETER_CAN_USE_MP11
}} // namespace methcla_boost::parameter

#include <boost/parameter/aux_/is_placeholder.hpp>

namespace methcla_boost { namespace parameter {

    template <
        typename Parameters
      , typename Keyword
      , typename Default = ::methcla_boost::parameter::void_
    >
    struct binding
#if !defined(BOOST_PARAMETER_CAN_USE_MP11)
      : ::methcla_boost::mpl::eval_if<
            ::methcla_boost::parameter::aux::is_mpl_placeholder<Parameters>
          , ::methcla_boost::mpl::identity<int>
          , ::methcla_boost::parameter::binding0<Parameters,Keyword,Default>
        >
#endif
    {
#if defined(BOOST_PARAMETER_CAN_USE_MP11)
        using type = typename ::methcla_boost::mp11::mp_if<
            ::methcla_boost::parameter::aux::is_mpl_placeholder<Parameters>
          , ::methcla_boost::mp11::mp_identity<int>
          , ::methcla_boost::mp11::mp_if<
                ::methcla_boost::parameter::aux::is_mp11_placeholder<Parameters>
              , ::methcla_boost::parameter::binding1<Parameters,Keyword,Default>
              , ::methcla_boost::parameter::binding0<Parameters,Keyword,Default>
            >
        >::type;
#endif
    };
}} // namespace methcla_boost::parameter

#include <boost/parameter/aux_/result_of0.hpp>

namespace methcla_boost { namespace parameter {

    // A metafunction that, given an argument pack, returns the reference type
    // of the parameter identified by the given keyword.  If no such parameter
    // has been specified, returns the type returned by invoking DefaultFn
    template <typename Parameters, typename Keyword, typename DefaultFn>
    struct lazy_binding
    {
#if defined(BOOST_PARAMETER_CAN_USE_MP11)
        using type = ::methcla_boost::mp11::mp_apply_q<
            typename Parameters::binding
          , ::methcla_boost::mp11::mp_list<
                Keyword
              , typename ::methcla_boost::parameter::aux::result_of0<DefaultFn>::type
              , ::methcla_boost::mp11::mp_true
            >
        >;
#else
        typedef typename ::methcla_boost::mpl::apply_wrap3<
            typename Parameters::binding
          , Keyword
          , typename ::methcla_boost::parameter::aux::result_of0<DefaultFn>::type
          , ::methcla_boost::mpl::true_
        >::type type;
#endif  // BOOST_PARAMETER_CAN_USE_MP11
    };
}} // namespace methcla_boost::parameter

#endif  // include guard

