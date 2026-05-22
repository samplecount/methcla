//////////////////////////////////////////////////////////////////////////////
//
// (C) Copyright Ion Gaztanaga 2008-2012. Distributed under the Boost
// Software License, Version 1.0. (See accompanying file
// LICENSE_1_0.txt or copy at http://www.boost.org/LICENSE_1_0.txt)
//
// See http://www.boost.org/libs/interprocess for documentation.
//
//////////////////////////////////////////////////////////////////////////////

#ifndef BOOST_INTERPROCESS_CONTAINERS_ALLOCATION_TYPE_HPP
#define BOOST_INTERPROCESS_CONTAINERS_ALLOCATION_TYPE_HPP

#ifndef BOOST_CONFIG_HPP
#  include <boost/config.hpp>
#endif
#
#if defined(BOOST_HAS_PRAGMA_ONCE)
#  pragma once
#endif

#include <boost/interprocess/detail/config_begin.hpp>
#include <boost/container/detail/allocation_type.hpp>

namespace methcla_boost {
namespace interprocess {

#if !defined(BOOST_INTERPROCESS_DOXYGEN_INVOKED)
using methcla_boost::container::allocation_type;
#endif   //#ifndef BOOST_INTERPROCESS_DOXYGEN_INVOKED
static const allocation_type allocate_new       = methcla_boost::container::allocate_new;
static const allocation_type expand_fwd         = methcla_boost::container::expand_fwd;
static const allocation_type expand_bwd         = methcla_boost::container::expand_bwd;
static const allocation_type shrink_in_place    = methcla_boost::container::shrink_in_place;
static const allocation_type try_shrink_in_place= methcla_boost::container::try_shrink_in_place;
static const allocation_type nothrow_allocation = methcla_boost::container::nothrow_allocation;
static const allocation_type zero_memory        = methcla_boost::container::zero_memory;

}  //namespace interprocess {
}  //namespace methcla_boost {

#include <boost/interprocess/detail/config_end.hpp>

#endif //   #ifndef  BOOST_INTERPROCESS_CONTAINERS_ALLOCATION_TYPE_HPP
