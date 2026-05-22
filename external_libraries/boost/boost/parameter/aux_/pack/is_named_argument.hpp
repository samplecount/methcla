// Copyright David Abrahams, Daniel Wallin 2003.
// Distributed under the Boost Software License, Version 1.0.
// (See accompanying file LICENSE_1_0.txt or copy at
// http://www.boost.org/LICENSE_1_0.txt)

#ifndef BOOST_PARAMETER_AUX_PACK_IS_NAMED_ARGUMENT_HPP
#define BOOST_PARAMETER_AUX_PACK_IS_NAMED_ARGUMENT_HPP

#include <boost/parameter/aux_/template_keyword.hpp>
#include <boost/parameter/aux_/is_tagged_argument.hpp>
#include <boost/parameter/config.hpp>

#if defined(BOOST_PARAMETER_CAN_USE_MP11)
#include <boost/mp11/integral.hpp>
#include <boost/mp11/utility.hpp>
#else
#include <boost/mpl/bool.hpp>
#include <boost/mpl/if.hpp>
#endif

namespace methcla_boost { namespace parameter { namespace aux {

    template <typename T>
#if defined(BOOST_PARAMETER_CAN_USE_MP11)
    using is_named_argument = ::methcla_boost::mp11::mp_if<
        ::methcla_boost::parameter::aux::is_template_keyword<T>
      , ::methcla_boost::mp11::mp_true
      , ::methcla_boost::parameter::aux::is_tagged_argument_mp11<T>
    >;
#else
    struct is_named_argument
      : ::methcla_boost::mpl::if_<
            ::methcla_boost::parameter::aux::is_template_keyword<T>
          , ::methcla_boost::mpl::true_
          , ::methcla_boost::parameter::aux::is_tagged_argument<T>
        >::type
    {
    };
#endif
}}} // namespace methcla_boost::parameter::aux

#endif  // include guard

