// Copyright Daniel Wallin, David Abrahams 2005.
// Copyright Cromwell D. Enage 2017.
// Distributed under the Boost Software License, Version 1.0.
// (See accompanying file LICENSE_1_0.txt or copy at
// http://www.boost.org/LICENSE_1_0.txt)

#ifndef BOOST_PARAMETER_KEYWORD_HPP
#define BOOST_PARAMETER_KEYWORD_HPP

#include <boost/parameter/aux_/tag.hpp>
#include <boost/parameter/aux_/default.hpp>
#include <boost/parameter/keyword_fwd.hpp>
#include <boost/parameter/config.hpp>

#if defined(BOOST_PARAMETER_HAS_PERFECT_FORWARDING)
#include <boost/core/enable_if.hpp>
#include <utility>

#if defined(BOOST_PARAMETER_CAN_USE_MP11)
#include <boost/mp11/integral.hpp>
#include <boost/mp11/utility.hpp>
#include <type_traits>
#else
#include <boost/mpl/bool.hpp>
#include <boost/mpl/if.hpp>
#include <boost/mpl/eval_if.hpp>
#include <boost/type_traits/is_same.hpp>
#include <boost/type_traits/is_scalar.hpp>
#include <boost/type_traits/is_const.hpp>
#endif

namespace methcla_boost { namespace parameter {

    // Instances of unique specializations of keyword<...> serve to
    // associate arguments with parameter names.  For example:
    //
    //     struct rate_;             // parameter names
    //     struct skew_;
    //
    //     namespace
    //     {
    //         keyword<rate_> rate;  // keywords
    //         keyword<skew_> skew;
    //     }
    //
    //     ...
    //
    //     f(rate = 1, skew = 2.4);
    template <typename Tag>
    struct keyword
    {
        typedef Tag tag;

        inline BOOST_CONSTEXPR keyword()
        {
        }

        template <typename T>
        inline BOOST_CONSTEXPR typename ::methcla_boost::lazy_enable_if<
#if defined(BOOST_PARAMETER_CAN_USE_MP11)
            ::methcla_boost::mp11::mp_if<
                ::std::is_scalar<T>
              , ::methcla_boost::mp11::mp_true
              , ::methcla_boost::mp11::mp_if<
                    ::std::is_same<
                        typename Tag::qualifier
                      , ::methcla_boost::parameter::in_reference
                    >
                  , ::methcla_boost::mp11::mp_true
                  , ::std::is_same<
                        typename Tag::qualifier
                      , ::methcla_boost::parameter::forward_reference
                    >
                >
            >
#else   // !defined(BOOST_PARAMETER_CAN_USE_MP11)
            typename ::methcla_boost::mpl::eval_if<
                ::methcla_boost::is_scalar<T>
              , ::methcla_boost::mpl::true_
              , ::methcla_boost::mpl::eval_if<
                    ::methcla_boost::is_same<
                        typename Tag::qualifier
                      , ::methcla_boost::parameter::in_reference
                    >
                  , ::methcla_boost::mpl::true_
                  , ::methcla_boost::mpl::if_<
                        ::methcla_boost::is_same<
                            typename Tag::qualifier
                          , ::methcla_boost::parameter::forward_reference
                        >
                      , ::methcla_boost::mpl::true_
                      , ::methcla_boost::mpl::false_
                    >
                >
            >::type
#endif  // BOOST_PARAMETER_CAN_USE_MP11
          , ::methcla_boost::parameter::aux::tag<Tag,T const&>
        >::type
            operator=(T const& x) const
        {
            typedef typename ::methcla_boost::parameter::aux
            ::tag<Tag,T const&>::type result;
            return result(x);
        }

        template <typename Default>
        inline BOOST_CONSTEXPR typename ::methcla_boost::enable_if<
#if defined(BOOST_PARAMETER_CAN_USE_MP11)
            ::methcla_boost::mp11::mp_if<
                ::std::is_scalar<Default>
              , ::methcla_boost::mp11::mp_true
              , ::methcla_boost::mp11::mp_if<
                    ::std::is_same<
                        typename Tag::qualifier
                      , ::methcla_boost::parameter::in_reference
                    >
                  , ::methcla_boost::mp11::mp_true
                  , ::std::is_same<
                        typename Tag::qualifier
                      , ::methcla_boost::parameter::forward_reference
                    >
                >
            >
#else   // !defined(BOOST_PARAMETER_CAN_USE_MP11)
            typename ::methcla_boost::mpl::eval_if<
                ::methcla_boost::is_scalar<Default>
              , ::methcla_boost::mpl::true_
              , ::methcla_boost::mpl::eval_if<
                    ::methcla_boost::is_same<
                        typename Tag::qualifier
                      , ::methcla_boost::parameter::in_reference
                    >
                  , ::methcla_boost::mpl::true_
                  , ::methcla_boost::mpl::if_<
                        ::methcla_boost::is_same<
                            typename Tag::qualifier
                          , ::methcla_boost::parameter::forward_reference
                        >
                      , ::methcla_boost::mpl::true_
                      , ::methcla_boost::mpl::false_
                    >
                >
            >::type
#endif  // BOOST_PARAMETER_CAN_USE_MP11
          , ::methcla_boost::parameter::aux::default_<Tag,Default const>
        >::type
            operator|(Default const& d) const
        {
            return ::methcla_boost::parameter::aux::default_<Tag,Default const>(d);
        }

        template <typename T>
        inline BOOST_CONSTEXPR typename ::methcla_boost::lazy_enable_if<
#if defined(BOOST_PARAMETER_CAN_USE_MP11)
            ::methcla_boost::mp11::mp_if<
                ::methcla_boost::mp11::mp_if<
                    ::std::is_same<
                        typename Tag::qualifier
                      , ::methcla_boost::parameter::out_reference
                    >
                  , ::methcla_boost::mp11::mp_true
                  , ::std::is_same<
                        typename Tag::qualifier
                      , ::methcla_boost::parameter::forward_reference
                    >
                >
              , ::methcla_boost::mp11::mp_if<
                    ::std::is_const<T>
                  , ::methcla_boost::mp11::mp_false
                  , ::methcla_boost::mp11::mp_true
                >
              , ::methcla_boost::mp11::mp_false
            >
#else   // !defined(BOOST_PARAMETER_CAN_USE_MP11)
            typename ::methcla_boost::mpl::eval_if<
                typename ::methcla_boost::mpl::if_<
                    ::methcla_boost::is_same<
                        typename Tag::qualifier
                      , ::methcla_boost::parameter::out_reference
                    >
                  , ::methcla_boost::mpl::true_
                  , ::methcla_boost::is_same<
                        typename Tag::qualifier
                      , ::methcla_boost::parameter::forward_reference
                    >
                >::type
              , ::methcla_boost::mpl::if_<
                    ::methcla_boost::is_const<T>
                  , ::methcla_boost::mpl::false_
                  , ::methcla_boost::mpl::true_
                >
              , ::methcla_boost::mpl::false_
            >::type
#endif  // BOOST_PARAMETER_CAN_USE_MP11
          , ::methcla_boost::parameter::aux::tag<Tag,T&>
        >::type
            operator=(T& x) const
        {
            typedef typename ::methcla_boost::parameter::aux
            ::tag<Tag,T&>::type result;
            return result(x);
        }

        template <typename Default>
        inline BOOST_CONSTEXPR typename ::methcla_boost::enable_if<
#if defined(BOOST_PARAMETER_CAN_USE_MP11)
            ::methcla_boost::mp11::mp_if<
                ::methcla_boost::mp11::mp_if<
                    ::std::is_same<
                        typename Tag::qualifier
                      , ::methcla_boost::parameter::out_reference
                    >
                  , ::methcla_boost::mp11::mp_true
                  , ::std::is_same<
                        typename Tag::qualifier
                      , ::methcla_boost::parameter::forward_reference
                    >
                >
              , ::methcla_boost::mp11::mp_if<
                    ::std::is_const<Default>
                  , ::methcla_boost::mp11::mp_false
                  , ::methcla_boost::mp11::mp_true
                >
              , ::methcla_boost::mp11::mp_false
            >
#else   // !defined(BOOST_PARAMETER_CAN_USE_MP11)
            typename ::methcla_boost::mpl::eval_if<
                typename ::methcla_boost::mpl::if_<
                    ::methcla_boost::is_same<
                        typename Tag::qualifier
                      , ::methcla_boost::parameter::out_reference
                    >
                  , ::methcla_boost::mpl::true_
                  , ::methcla_boost::is_same<
                        typename Tag::qualifier
                      , ::methcla_boost::parameter::forward_reference
                    >
                >::type
              , ::methcla_boost::mpl::if_<
                    ::methcla_boost::is_const<Default>
                  , ::methcla_boost::mpl::false_
                  , ::methcla_boost::mpl::true_
                >
              , ::methcla_boost::mpl::false_
            >::type
#endif  // BOOST_PARAMETER_CAN_USE_MP11
          , ::methcla_boost::parameter::aux::default_<Tag,Default>
        >::type
            operator|(Default& d) const
        {
            return ::methcla_boost::parameter::aux::default_<Tag,Default>(d);
        }

        template <typename Default>
        inline BOOST_CONSTEXPR
        ::methcla_boost::parameter::aux::lazy_default<Tag,Default const>
            operator||(Default const& d) const
        {
            return ::methcla_boost::parameter::aux
            ::lazy_default<Tag,Default const>(d);
        }

        template <typename Default>
        inline BOOST_CONSTEXPR
        ::methcla_boost::parameter::aux::lazy_default<Tag,Default>
            operator||(Default& d) const
        {
            return ::methcla_boost::parameter::aux::lazy_default<Tag,Default>(d);
        }

        template <typename T>
        inline BOOST_CONSTEXPR typename ::methcla_boost::lazy_enable_if<
#if defined(BOOST_PARAMETER_CAN_USE_MP11)
            ::methcla_boost::mp11::mp_if<
                ::std::is_scalar<T>
              , ::methcla_boost::mp11::mp_false
              , ::methcla_boost::mp11::mp_if<
                    ::std::is_same<
                        typename Tag::qualifier
                      , ::methcla_boost::parameter::in_reference
                    >
                  , ::methcla_boost::mp11::mp_true
                  , ::std::is_same<
                        typename Tag::qualifier
                      , ::methcla_boost::parameter::forward_reference
                    >
                >
            >
#else   // !defined(BOOST_PARAMETER_CAN_USE_MP11)
            typename ::methcla_boost::mpl::eval_if<
                ::methcla_boost::is_scalar<T>
              , ::methcla_boost::mpl::false_
              , ::methcla_boost::mpl::eval_if<
                    ::methcla_boost::is_same<
                        typename Tag::qualifier
                      , ::methcla_boost::parameter::in_reference
                    >
                  , ::methcla_boost::mpl::true_
                  , ::methcla_boost::mpl::if_<
                        ::methcla_boost::is_same<
                            typename Tag::qualifier
                          , ::methcla_boost::parameter::forward_reference
                        >
                      , ::methcla_boost::mpl::true_
                      , ::methcla_boost::mpl::false_
                    >
                >
            >::type
#endif  // BOOST_PARAMETER_CAN_USE_MP11
          , ::methcla_boost::parameter::aux::tag<Tag,T const>
        >::type
            operator=(T const&& x) const
        {
            typedef typename ::methcla_boost::parameter::aux
            ::tag<Tag,T const>::type result;
            return result(::std::forward<T const>(x));
        }

        template <typename T>
        inline BOOST_CONSTEXPR typename ::methcla_boost::lazy_enable_if<
#if defined(BOOST_PARAMETER_CAN_USE_MP11)
            ::methcla_boost::mp11::mp_if<
                ::std::is_scalar<T>
              , ::methcla_boost::mp11::mp_false
              , ::methcla_boost::mp11::mp_if<
                    ::std::is_same<
                        typename Tag::qualifier
                      , ::methcla_boost::parameter::consume_reference
                    >
                  , ::methcla_boost::mp11::mp_true
                  , ::std::is_same<
                        typename Tag::qualifier
                      , ::methcla_boost::parameter::forward_reference
                    >
                >
            >
#else   // !defined(BOOST_PARAMETER_CAN_USE_MP11)
            typename ::methcla_boost::mpl::eval_if<
                ::methcla_boost::is_scalar<T>
              , ::methcla_boost::mpl::false_
              , ::methcla_boost::mpl::eval_if<
                    ::methcla_boost::is_same<
                        typename Tag::qualifier
                      , ::methcla_boost::parameter::consume_reference
                    >
                  , ::methcla_boost::mpl::true_
                  , ::methcla_boost::mpl::if_<
                        ::methcla_boost::is_same<
                            typename Tag::qualifier
                          , ::methcla_boost::parameter::forward_reference
                        >
                      , ::methcla_boost::mpl::true_
                      , ::methcla_boost::mpl::false_
                    >
                >
            >::type
#endif  // BOOST_PARAMETER_CAN_USE_MP11
          , ::methcla_boost::parameter::aux::tag<Tag,T>
        >::type
            operator=(T&& x) const
        {
            typedef typename ::methcla_boost::parameter::aux::tag<Tag,T>::type result;
            return result(::std::forward<T>(x));
        }

        template <typename Default>
        inline BOOST_CONSTEXPR typename ::methcla_boost::enable_if<
#if defined(BOOST_PARAMETER_CAN_USE_MP11)
            ::methcla_boost::mp11::mp_if<
                ::std::is_scalar<Default>
              , ::methcla_boost::mp11::mp_false
              , ::methcla_boost::mp11::mp_if<
                    ::std::is_same<
                        typename Tag::qualifier
                      , ::methcla_boost::parameter::in_reference
                    >
                  , ::methcla_boost::mp11::mp_true
                  , ::std::is_same<
                        typename Tag::qualifier
                      , ::methcla_boost::parameter::forward_reference
                    >
                >
            >
#else   // !defined(BOOST_PARAMETER_CAN_USE_MP11)
            typename ::methcla_boost::mpl::eval_if<
                ::methcla_boost::is_scalar<Default>
              , ::methcla_boost::mpl::false_
              , ::methcla_boost::mpl::eval_if<
                    ::methcla_boost::is_same<
                        typename Tag::qualifier
                      , ::methcla_boost::parameter::in_reference
                    >
                  , ::methcla_boost::mpl::true_
                  , ::methcla_boost::mpl::if_<
                        ::methcla_boost::is_same<
                            typename Tag::qualifier
                          , ::methcla_boost::parameter::forward_reference
                        >
                      , ::methcla_boost::mpl::true_
                      , ::methcla_boost::mpl::false_
                    >
                >
            >::type
#endif  // BOOST_PARAMETER_CAN_USE_MP11
          , ::methcla_boost::parameter::aux::default_r_<Tag,Default const>
        >::type
            operator|(Default const&& d) const
        {
            return ::methcla_boost::parameter::aux::default_r_<Tag,Default const>(
                ::std::forward<Default const>(d)
            );
        }

        template <typename Default>
        inline BOOST_CONSTEXPR typename ::methcla_boost::enable_if<
#if defined(BOOST_PARAMETER_CAN_USE_MP11)
            ::methcla_boost::mp11::mp_if<
                ::std::is_scalar<Default>
              , ::methcla_boost::mp11::mp_false
              , ::methcla_boost::mp11::mp_if<
                    ::std::is_same<
                        typename Tag::qualifier
                      , ::methcla_boost::parameter::consume_reference
                    >
                  , ::methcla_boost::mp11::mp_true
                  , ::std::is_same<
                        typename Tag::qualifier
                      , ::methcla_boost::parameter::forward_reference
                    >
                >
            >
#else   // !defined(BOOST_PARAMETER_CAN_USE_MP11)
            typename ::methcla_boost::mpl::eval_if<
                ::methcla_boost::is_scalar<Default>
              , ::methcla_boost::mpl::false_
              , ::methcla_boost::mpl::eval_if<
                    ::methcla_boost::is_same<
                        typename Tag::qualifier
                      , ::methcla_boost::parameter::consume_reference
                    >
                  , ::methcla_boost::mpl::true_
                  , ::methcla_boost::mpl::if_<
                        ::methcla_boost::is_same<
                            typename Tag::qualifier
                          , ::methcla_boost::parameter::forward_reference
                        >
                      , ::methcla_boost::mpl::true_
                      , ::methcla_boost::mpl::false_
                    >
                >
            >::type
#endif  // BOOST_PARAMETER_CAN_USE_MP11
          , ::methcla_boost::parameter::aux::default_r_<Tag,Default>
        >::type
            operator|(Default&& d) const
        {
            return ::methcla_boost::parameter::aux
            ::default_r_<Tag,Default>(::std::forward<Default>(d));
        }

     public: // Insurance against ODR violations
        // Users will need to define their keywords in header files.  To
        // prevent ODR violations, it's important that the keyword used in
        // every instantiation of a function template is the same object.
        // We provide a reference to a common instance of each keyword
        // object and prevent construction by users.
        static ::methcla_boost::parameter::keyword<Tag> const instance;

        // This interface is deprecated.
        static ::methcla_boost::parameter::keyword<Tag>& get()
        {
            return const_cast< ::methcla_boost::parameter::keyword<Tag>&>(instance);
        }
    };

    template <typename Tag>
    ::methcla_boost::parameter::keyword<Tag> const ::methcla_boost::parameter
    ::keyword<Tag>::instance = ::methcla_boost::parameter::keyword<Tag>();
}} // namespace methcla_boost::parameter

#else   // !defined(BOOST_PARAMETER_HAS_PERFECT_FORWARDING)

#if !defined(BOOST_NO_SFINAE)
#include <boost/mpl/bool.hpp>
#include <boost/mpl/if.hpp>
#include <boost/mpl/eval_if.hpp>
#include <boost/core/enable_if.hpp>
#include <boost/type_traits/is_same.hpp>
#include <boost/type_traits/is_scalar.hpp>
#include <boost/type_traits/is_const.hpp>
#endif  // BOOST_NO_SFINAE

namespace methcla_boost { namespace parameter {

    // Instances of unique specializations of keyword<...> serve to
    // associate arguments with parameter names.  For example:
    //
    //     struct rate_;             // parameter names
    //     struct skew_;
    //
    //     namespace
    //     {
    //         keyword<rate_> rate;  // keywords
    //         keyword<skew_> skew;
    //     }
    //
    //     ...
    //
    //     f(rate = 1, skew = 2.4);
    template <typename Tag>
    struct keyword
    {
        typedef Tag tag;

        inline BOOST_CONSTEXPR keyword()
        {
        }

        template <typename T>
#if defined(BOOST_NO_SFINAE)
        inline typename ::methcla_boost::parameter::aux::tag<Tag,T const&>::type
#else
        inline BOOST_CONSTEXPR typename ::methcla_boost::lazy_enable_if<
            typename ::methcla_boost::mpl::eval_if<
                ::methcla_boost::is_scalar<T>
              , ::methcla_boost::mpl::true_
              , ::methcla_boost::mpl::eval_if<
                    ::methcla_boost::is_same<
                        typename Tag::qualifier
                      , ::methcla_boost::parameter::in_reference
                    >
                  , ::methcla_boost::mpl::true_
                  , ::methcla_boost::mpl::if_<
                        ::methcla_boost::is_same<
                            typename Tag::qualifier
                          , ::methcla_boost::parameter::forward_reference
                        >
                      , ::methcla_boost::mpl::true_
                      , ::methcla_boost::mpl::false_
                    >
                >
            >::type
          , ::methcla_boost::parameter::aux::tag<Tag,T const&>
        >::type
#endif  // BOOST_NO_SFINAE
            operator=(T const& x) const
        {
            typedef typename ::methcla_boost::parameter::aux
            ::tag<Tag,T const&>::type result;
            return result(x);
        }

        template <typename Default>
#if defined(BOOST_NO_SFINAE)
        inline ::methcla_boost::parameter::aux::default_<Tag,Default const>
#else
        inline BOOST_CONSTEXPR typename ::methcla_boost::enable_if<
            typename ::methcla_boost::mpl::eval_if<
                ::methcla_boost::is_scalar<Default>
              , ::methcla_boost::mpl::true_
              , ::methcla_boost::mpl::eval_if<
                    ::methcla_boost::is_same<
                        typename Tag::qualifier
                      , ::methcla_boost::parameter::in_reference
                    >
                  , ::methcla_boost::mpl::true_
                  , ::methcla_boost::mpl::if_<
                        ::methcla_boost::is_same<
                            typename Tag::qualifier
                          , ::methcla_boost::parameter::forward_reference
                        >
                      , ::methcla_boost::mpl::true_
                      , ::methcla_boost::mpl::false_
                    >
                >
            >::type
          , ::methcla_boost::parameter::aux::default_<Tag,Default const>
        >::type
#endif  // BOOST_NO_SFINAE
            operator|(Default const& d) const
        {
            return ::methcla_boost::parameter::aux::default_<Tag,Default const>(d);
        }

        template <typename T>
#if defined(BOOST_NO_SFINAE)
        inline typename ::methcla_boost::parameter::aux::tag<Tag,T&>::type
#else
        inline BOOST_CONSTEXPR typename ::methcla_boost::lazy_enable_if<
            typename ::methcla_boost::mpl::eval_if<
                typename ::methcla_boost::mpl::if_<
                    ::methcla_boost::is_same<
                        typename Tag::qualifier
                      , ::methcla_boost::parameter::out_reference
                    >
                  , ::methcla_boost::mpl::true_
                  , ::methcla_boost::is_same<
                        typename Tag::qualifier
                      , ::methcla_boost::parameter::forward_reference
                    >
                >::type
              , ::methcla_boost::mpl::if_<
                    ::methcla_boost::is_const<T>
                  , ::methcla_boost::mpl::false_
                  , ::methcla_boost::mpl::true_
                >
              , ::methcla_boost::mpl::false_
            >::type
          , ::methcla_boost::parameter::aux::tag<Tag,T&>
        >::type
#endif  // BOOST_NO_SFINAE
            operator=(T& x) const
        {
            typedef typename ::methcla_boost::parameter::aux
            ::tag<Tag,T&>::type result;
            return result(x);
        }

        template <typename Default>
#if defined(BOOST_NO_SFINAE)
        inline ::methcla_boost::parameter::aux::default_<Tag,Default>
#else
        inline BOOST_CONSTEXPR typename ::methcla_boost::enable_if<
            typename ::methcla_boost::mpl::eval_if<
                typename ::methcla_boost::mpl::if_<
                    ::methcla_boost::is_same<
                        typename Tag::qualifier
                      , ::methcla_boost::parameter::out_reference
                    >
                  , ::methcla_boost::mpl::true_
                  , ::methcla_boost::is_same<
                        typename Tag::qualifier
                      , ::methcla_boost::parameter::forward_reference
                    >
                >::type
              , ::methcla_boost::mpl::if_<
                    ::methcla_boost::is_const<Default>
                  , ::methcla_boost::mpl::false_
                  , ::methcla_boost::mpl::true_
                >
              , ::methcla_boost::mpl::false_
            >::type
          , ::methcla_boost::parameter::aux::default_<Tag,Default>
        >::type
#endif  // BOOST_NO_SFINAE
            operator|(Default& d) const
        {
            return ::methcla_boost::parameter::aux::default_<Tag,Default>(d);
        }

        template <typename Default>
        inline BOOST_CONSTEXPR
        ::methcla_boost::parameter::aux::lazy_default<Tag,Default const>
            operator||(Default const& d) const
        {
            return ::methcla_boost::parameter::aux
            ::lazy_default<Tag,Default const>(d);
        }

        template <typename Default>
        inline BOOST_CONSTEXPR
        ::methcla_boost::parameter::aux::lazy_default<Tag,Default>
            operator||(Default& d) const
        {
            return ::methcla_boost::parameter::aux::lazy_default<Tag,Default>(d);
        }

     public: // Insurance against ODR violations
        // Users will need to define their keywords in header files.  To
        // prevent ODR violations, it's important that the keyword used in
        // every instantiation of a function template is the same object.
        // We provide a reference to a common instance of each keyword
        // object and prevent construction by users.
        static ::methcla_boost::parameter::keyword<Tag> const instance;

        // This interface is deprecated.
        static ::methcla_boost::parameter::keyword<Tag>& get()
        {
            return const_cast< ::methcla_boost::parameter::keyword<Tag>&>(instance);
        }
    };

    template <typename Tag>
    ::methcla_boost::parameter::keyword<Tag> const ::methcla_boost::parameter
    ::keyword<Tag>::instance = ::methcla_boost::parameter::keyword<Tag>();
}} // namespace methcla_boost::parameter

#endif  // BOOST_PARAMETER_HAS_PERFECT_FORWARDING

#include <boost/parameter/aux_/name.hpp>
#include <boost/preprocessor/stringize.hpp>

// Reduces boilerplate required to declare and initialize keywords without
// violating ODR.  Declares a keyword tag type with the given name in
// namespace tag_namespace, and declares and initializes a reference in an
// anonymous namespace to a singleton instance of that type.
#if defined(BOOST_PARAMETER_CAN_USE_MP11)
#define BOOST_PARAMETER_KEYWORD(tag_namespace, name)                         \
    namespace tag_namespace                                                  \
    {                                                                        \
        struct name                                                          \
        {                                                                    \
            static BOOST_CONSTEXPR char const* keyword_name()                \
            {                                                                \
                return BOOST_PP_STRINGIZE(name);                             \
            }                                                                \
            using _ = BOOST_PARAMETER_TAG_PLACEHOLDER_TYPE(name);            \
            using _1 = _;                                                    \
            BOOST_PARAMETER_TAG_MP11_PLACEHOLDER_BINDING(binding_fn, name);  \
            BOOST_PARAMETER_TAG_MP11_PLACEHOLDER_VALUE(fn, name);            \
            using qualifier = ::methcla_boost::parameter::forward_reference;         \
        };                                                                   \
    }                                                                        \
    namespace                                                                \
    {                                                                        \
        ::methcla_boost::parameter::keyword<tag_namespace::name> const& name         \
            = ::methcla_boost::parameter::keyword<tag_namespace::name>::instance;    \
    }
/**/
#else   // !defined(BOOST_PARAMETER_CAN_USE_MP11)
#define BOOST_PARAMETER_KEYWORD(tag_namespace, name)                         \
    namespace tag_namespace                                                  \
    {                                                                        \
        struct name                                                          \
        {                                                                    \
            static BOOST_CONSTEXPR char const* keyword_name()                \
            {                                                                \
                return BOOST_PP_STRINGIZE(name);                             \
            }                                                                \
            typedef BOOST_PARAMETER_TAG_PLACEHOLDER_TYPE(name) _;            \
            typedef BOOST_PARAMETER_TAG_PLACEHOLDER_TYPE(name) _1;           \
            typedef ::methcla_boost::parameter::forward_reference qualifier;         \
        };                                                                   \
    }                                                                        \
    namespace                                                                \
    {                                                                        \
        ::methcla_boost::parameter::keyword<tag_namespace::name> const& name         \
            = ::methcla_boost::parameter::keyword<tag_namespace::name>::instance;    \
    }
/**/
#endif  // BOOST_PARAMETER_CAN_USE_MP11

#endif  // include guard

