// Copyright Daniel Wallin, David Abrahams 2005.
// Distributed under the Boost Software License, Version 1.0.
// (See accompanying file LICENSE_1_0.txt or copy at
// http://www.boost.org/LICENSE_1_0.txt)

#ifndef BOOST_PARAMETER_VOID_050329_HPP
#define BOOST_PARAMETER_VOID_050329_HPP

namespace methcla_boost { namespace parameter {

    // A placemarker for "no argument passed."
    // MAINTAINER NOTE: Do not make this into a metafunction
    struct void_
    {
    };
}} // namespace methcla_boost::parameter

namespace methcla_boost { namespace parameter { namespace aux {

    inline ::methcla_boost::parameter::void_& void_reference()
    {
        static ::methcla_boost::parameter::void_ instance;
        return instance;
    }
}}} // namespace methcla_boost::parameter::aux

#include <boost/config/workaround.hpp>

#if BOOST_WORKAROUND(__SUNPRO_CC, BOOST_TESTED_AT(0x580))

namespace methcla_boost { namespace parameter { namespace aux {

    typedef void* voidstar;
}}} // namespace methcla_boost::parameter::aux

#endif
#endif  // include guard

