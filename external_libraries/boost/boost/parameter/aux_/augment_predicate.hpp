// Copyright Cromwell D. Enage 2018.
// Distributed under the Boost Software License, Version 1.0.
// (See accompanying file LICENSE_1_0.txt or copy at
// http://www.boost.org/LICENSE_1_0.txt)

#ifndef BOOST_PARAMETER_AUGMENT_PREDICATE_HPP
#define BOOST_PARAMETER_AUGMENT_PREDICATE_HPP

#include <boost/parameter/keyword_fwd.hpp>
#include <boost/mpl/bool.hpp>
#include <boost/mpl/if.hpp>
#include <boost/mpl/eval_if.hpp>
#include <boost/type_traits/is_lvalue_reference.hpp>
#include <boost/type_traits/is_scalar.hpp>
#include <boost/type_traits/is_same.hpp>

namespace methcla_boost { namespace parameter { namespace aux {

    template <typename V, typename R, typename Tag>
    struct augment_predicate_check_consume_ref
      : ::methcla_boost::mpl::eval_if<
            ::methcla_boost::is_scalar<V>
          , ::methcla_boost::mpl::true_
          , ::methcla_boost::mpl::eval_if<
                ::methcla_boost::is_same<
                    typename Tag::qualifier
                  , ::methcla_boost::parameter::consume_reference
                >
              , ::methcla_boost::mpl::if_<
                    ::methcla_boost::is_lvalue_reference<R>
                  , ::methcla_boost::mpl::false_
                  , ::methcla_boost::mpl::true_
                >
              , methcla_boost::mpl::true_
            >
        >::type
    {
    };
}}} // namespace methcla_boost::parameter::aux

#include <boost/type_traits/is_const.hpp>

namespace methcla_boost { namespace parameter { namespace aux {

    template <typename V, typename R, typename Tag>
    struct augment_predicate_check_out_ref
      : ::methcla_boost::mpl::eval_if<
            ::methcla_boost::is_same<
                typename Tag::qualifier
              , ::methcla_boost::parameter::out_reference
            >
          , ::methcla_boost::mpl::eval_if<
                ::methcla_boost::is_lvalue_reference<R>
              , ::methcla_boost::mpl::if_<
                    ::methcla_boost::is_const<V>
                  , ::methcla_boost::mpl::false_
                  , ::methcla_boost::mpl::true_
                >
              , ::methcla_boost::mpl::false_
            >
          , ::methcla_boost::mpl::true_
        >::type
    {
    };
}}} // namespace methcla_boost::parameter::aux

#include <boost/parameter/aux_/lambda_tag.hpp>
#include <boost/mpl/apply_wrap.hpp>
#include <boost/mpl/lambda.hpp>

namespace methcla_boost { namespace parameter { namespace aux {

    template <
        typename Predicate
      , typename R
      , typename Tag
      , typename T
      , typename Args
    >
    class augment_predicate
    {
        typedef typename ::methcla_boost::mpl::lambda<
            Predicate
          , ::methcla_boost::parameter::aux::lambda_tag
        >::type _actual_predicate;

     public:
        typedef typename ::methcla_boost::mpl::eval_if<
            typename ::methcla_boost::mpl::if_<
                ::methcla_boost::parameter::aux
                ::augment_predicate_check_consume_ref<T,R,Tag>
              , ::methcla_boost::parameter::aux
                ::augment_predicate_check_out_ref<T,R,Tag>
              , ::methcla_boost::mpl::false_
            >::type
          , ::methcla_boost::mpl::apply_wrap2<_actual_predicate,T,Args>
          , ::methcla_boost::mpl::false_
        >::type type;
    };
}}} // namespace methcla_boost::parameter::aux

#include <boost/parameter/config.hpp>

#if defined(BOOST_PARAMETER_CAN_USE_MP11)
#include <boost/mp11/integral.hpp>
#include <boost/mp11/utility.hpp>
#include <type_traits>

namespace methcla_boost { namespace parameter { namespace aux {

    template <typename V, typename R, typename Tag>
    using augment_predicate_check_consume_ref_mp11 = ::methcla_boost::mp11::mp_if<
        ::std::is_scalar<V>
      , ::methcla_boost::mp11::mp_true
      , ::methcla_boost::mp11::mp_if<
            ::std::is_same<
                typename Tag::qualifier
              , ::methcla_boost::parameter::consume_reference
            >
          , ::methcla_boost::mp11::mp_if<
                ::std::is_lvalue_reference<R>
              , ::methcla_boost::mp11::mp_false
              , ::methcla_boost::mp11::mp_true
            >
          , methcla_boost::mp11::mp_true
        >
    >;

    template <typename V, typename R, typename Tag>
    using augment_predicate_check_out_ref_mp11 = ::methcla_boost::mp11::mp_if<
        ::std::is_same<
            typename Tag::qualifier
          , ::methcla_boost::parameter::out_reference
        >
      , ::methcla_boost::mp11::mp_if<
            ::std::is_lvalue_reference<R>
          , ::methcla_boost::mp11::mp_if<
                ::std::is_const<V>
              , ::methcla_boost::mp11::mp_false
              , ::methcla_boost::mp11::mp_true
            >
          , ::methcla_boost::mp11::mp_false
        >
      , ::methcla_boost::mp11::mp_true
    >;
}}} // namespace methcla_boost::parameter::aux

#include <boost/mp11/list.hpp>

namespace methcla_boost { namespace parameter { namespace aux {

    template <
        typename Predicate
      , typename R
      , typename Tag
      , typename T
      , typename Args
    >
    struct augment_predicate_mp11_impl
    {
        using type = ::methcla_boost::mp11::mp_if<
            ::methcla_boost::mp11::mp_if<
                ::methcla_boost::parameter::aux
                ::augment_predicate_check_consume_ref_mp11<T,R,Tag>
              , ::methcla_boost::parameter::aux
                ::augment_predicate_check_out_ref_mp11<T,R,Tag>
              , ::methcla_boost::mp11::mp_false
            >
          , ::methcla_boost::mp11
            ::mp_apply_q<Predicate,::methcla_boost::mp11::mp_list<T,Args> >
          , ::methcla_boost::mp11::mp_false
        >;
    };
}}} // namespace methcla_boost::parameter::aux

#include <boost/parameter/aux_/has_nested_template_fn.hpp>

namespace methcla_boost { namespace parameter { namespace aux {

    template <
        typename Predicate
      , typename R
      , typename Tag
      , typename T
      , typename Args
    >
    using augment_predicate_mp11 = ::methcla_boost::mp11::mp_if<
        ::methcla_boost::parameter::aux::has_nested_template_fn<Predicate>
      , ::methcla_boost::parameter::aux
        ::augment_predicate_mp11_impl<Predicate,R,Tag,T,Args>
      , ::methcla_boost::parameter::aux
        ::augment_predicate<Predicate,R,Tag,T,Args>
    >;
}}} // namespace methcla_boost::parameter::aux

#endif  // BOOST_PARAMETER_CAN_USE_MP11
#endif  // include guard

