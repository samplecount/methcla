// Copyright David Abrahams, Daniel Wallin 2003.
// Distributed under the Boost Software License, Version 1.0.
// (See accompanying file LICENSE_1_0.txt or copy at
// http://www.boost.org/LICENSE_1_0.txt)

#ifndef BOOST_PARAMETER_AUX_PACK_SATISFIES_HPP
#define BOOST_PARAMETER_AUX_PACK_SATISFIES_HPP

#include <boost/parameter/config.hpp>

#if BOOST_WORKAROUND(BOOST_MSVC, == 1310)
#include <boost/parameter/aux_/arg_list.hpp>
#include <boost/parameter/aux_/augment_predicate.hpp>
#include <boost/parameter/aux_/void.hpp>
#include <boost/mpl/eval_if.hpp>
#include <boost/mpl/apply_wrap.hpp>
#include <boost/type_traits/is_same.hpp>
#else   // !BOOST_WORKAROUND(BOOST_MSVC, == 1310)
#if defined(BOOST_PARAMETER_CAN_USE_MP11)
#include <boost/mp11/integral.hpp>
#else
#include <boost/mpl/bool.hpp>
#endif
#include <boost/parameter/aux_/yesno.hpp>
#include <boost/parameter/aux_/preprocessor/nullptr.hpp>
#endif  // MSVC-7.1 workarounds needed

namespace methcla_boost { namespace parameter { namespace aux {

#if BOOST_WORKAROUND(BOOST_MSVC, == 1310)
    template <typename ArgList, typename ParameterRequirements, typename Bound>
    struct satisfies_impl
      : ::methcla_boost::parameter::aux::augment_predicate<
            typename ParameterRequirements::predicate
          , typename ArgList::reference
          , typename ArgList::key_type
          , Bound
          , ArgList
        >
    {
    };
#endif

    // Returns mpl::true_ iff the given ParameterRequirements are satisfied by
    // ArgList.
    template <typename ArgList, typename ParameterRequirements>
#if defined(BOOST_PARAMETER_CAN_USE_MP11)
    using satisfies = ::methcla_boost::mp11::mp_bool<
        sizeof(
            ::methcla_boost::parameter::aux::to_yesno(
                ArgList::satisfies(
                    static_cast<ParameterRequirements*>(
                        BOOST_PARAMETER_AUX_PP_NULLPTR
                    )
                  , static_cast<ArgList*>(BOOST_PARAMETER_AUX_PP_NULLPTR)
                )
            )
        ) == sizeof(::methcla_boost::parameter::aux::yes_tag)
    >;
#else   // !defined(BOOST_PARAMETER_CAN_USE_MP11)
    class satisfies
    {
#if BOOST_WORKAROUND(BOOST_MSVC, == 1310)
        // VC7.1 can't handle the sizeof() implementation below,
        // so we use this instead.
        typedef typename ::methcla_boost::mpl::apply_wrap3<
            typename ArgList::binding
          , typename ParameterRequirements::keyword
          , ::methcla_boost::parameter::void_
          , ::methcla_boost::mpl::false_
        >::type _bound;

     public:
        typedef typename ::methcla_boost::mpl::eval_if<
            ::methcla_boost::is_same<_bound,::methcla_boost::parameter::void_>
          , typename ParameterRequirements::has_default
          , ::methcla_boost::mpl::eval_if<
                ::methcla_boost::is_same<
                    ArgList
                  , ::methcla_boost::parameter::aux::empty_arg_list
                >
              , ::methcla_boost::mpl::false_
              , ::methcla_boost::parameter::aux::satisfies_impl<
                    ArgList
                  , ParameterRequirements
                  , _bound
                >
            >
        >::type type;
#else   // !BOOST_WORKAROUND(BOOST_MSVC, == 1310)
        BOOST_STATIC_CONSTANT(
            bool, _value = (
                sizeof(
                    ::methcla_boost::parameter::aux::to_yesno(
                        ArgList::satisfies(
                            static_cast<ParameterRequirements*>(
                                BOOST_PARAMETER_AUX_PP_NULLPTR
                            )
                          , static_cast<ArgList*>(BOOST_PARAMETER_AUX_PP_NULLPTR)
                        )
                    )
                ) == sizeof(::methcla_boost::parameter::aux::yes_tag)
            )
        );

     public:
        typedef ::methcla_boost::mpl::bool_<
            ::methcla_boost::parameter::aux
            ::satisfies<ArgList,ParameterRequirements>::_value
        > type;
#endif  // MSVC-7.1 workarounds needed
    };
#endif  // BOOST_PARAMETER_CAN_USE_MP11
}}} // namespace methcla_boost::parameter::aux

#include <boost/parameter/aux_/pack/as_parameter_requirements.hpp>

namespace methcla_boost { namespace parameter { namespace aux {

    // Returns mpl::true_ if the requirements of the given ParameterSpec
    // are satisfied by ArgList.
    template <typename ArgList, typename ParameterSpec>
#if defined(BOOST_PARAMETER_CAN_USE_MP11)
    using satisfies_requirements_of = ::methcla_boost::parameter::aux::satisfies<
        ArgList
      , typename ::methcla_boost::parameter::aux
        ::as_parameter_requirements<ParameterSpec>::type
    >;
#else
    struct satisfies_requirements_of
      : ::methcla_boost::parameter::aux::satisfies<
            ArgList
          , typename ::methcla_boost::parameter::aux
            ::as_parameter_requirements<ParameterSpec>::type
        >::type
    {
    };
#endif  // BOOST_PARAMETER_CAN_USE_MP11
}}} // namespace methcla_boost::parameter::aux

#endif  // include guard

