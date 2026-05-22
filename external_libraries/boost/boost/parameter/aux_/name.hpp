// Copyright Daniel Wallin 2006.
// Distributed under the Boost Software License, Version 1.0.
// (See accompanying file LICENSE_1_0.txt or copy at
// http://www.boost.org/LICENSE_1_0.txt)

#ifndef BOOST_PARAMETER_AUX_NAME_HPP
#define BOOST_PARAMETER_AUX_NAME_HPP

namespace methcla_boost { namespace parameter { namespace aux {

    struct name_tag_base
    {
    };

    template <typename Tag>
    struct name_tag
    {
    };
}}} // namespace methcla_boost::parameter::aux

#include <boost/mpl/bool.hpp>

namespace methcla_boost { namespace parameter { namespace aux {

    template <typename T>
    struct is_name_tag : ::methcla_boost::mpl::false_
    {
    };
}}} // namespace methcla_boost::parameter::aux

#include <boost/parameter/value_type.hpp>
#include <boost/mpl/placeholders.hpp>
#include <boost/config.hpp>
#include <boost/config/workaround.hpp>

#if !defined(BOOST_NO_SFINAE) && \
    !BOOST_WORKAROUND(BOOST_BORLANDC, BOOST_TESTED_AT(0x592))
#include <boost/parameter/aux_/lambda_tag.hpp>
#include <boost/mpl/lambda.hpp>
#include <boost/mpl/bind.hpp>
#include <boost/mpl/quote.hpp>
#include <boost/core/enable_if.hpp>

namespace methcla_boost { namespace mpl {

    template <typename T>
    struct lambda<
        T
      , typename ::methcla_boost::enable_if<
            ::methcla_boost::parameter::aux::is_name_tag<T>
          , ::methcla_boost::parameter::aux::lambda_tag
        >::type
    >
    {
        typedef ::methcla_boost::mpl::true_ is_le;
        typedef ::methcla_boost::mpl::bind3<
            ::methcla_boost::mpl::quote3< ::methcla_boost::parameter::value_type>
          , ::methcla_boost::mpl::arg<2>
          , T
          , void
        > result_;
        typedef result_ type;
    };
}} // namespace methcla_boost::mpl

#endif  // SFINAE enabled, not Borland.

#include <boost/parameter/aux_/void.hpp>

#define BOOST_PARAMETER_TAG_PLACEHOLDER_TYPE(tag)                            \
    ::methcla_boost::parameter::value_type<                                          \
        ::methcla_boost::mpl::_2,tag,::methcla_boost::parameter::void_                       \
    >
/**/

#if defined(BOOST_PARAMETER_CAN_USE_MP11)
#define BOOST_PARAMETER_TAG_MP11_PLACEHOLDER_VALUE(name, tag)                \
    template <typename ArgumentPack>                                         \
    using name = typename ::methcla_boost::parameter                                 \
    ::value_type<ArgumentPack,tag,::methcla_boost::parameter::void_>::type
/**/

#include <boost/parameter/binding.hpp>

#define BOOST_PARAMETER_TAG_MP11_PLACEHOLDER_BINDING(name, tag)              \
    template <typename ArgumentPack>                                         \
    using name = typename ::methcla_boost::parameter                                 \
    ::binding<ArgumentPack,tag,::methcla_boost::parameter::void_>::type
/**/

#endif  // BOOST_PARAMETER_CAN_USE_MP11
#endif  // include guard

