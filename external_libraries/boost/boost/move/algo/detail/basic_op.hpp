//////////////////////////////////////////////////////////////////////////////
//
// (C) Copyright Ion Gaztanaga 2015-2016.
// Distributed under the Boost Software License, Version 1.0.
// (See accompanying file LICENSE_1_0.txt or copy at
// http://www.boost.org/LICENSE_1_0.txt)
//
// See http://www.boost.org/libs/move for documentation.
//
//////////////////////////////////////////////////////////////////////////////
#ifndef BOOST_MOVE_ALGO_BASIC_OP
#define BOOST_MOVE_ALGO_BASIC_OP

#ifndef BOOST_CONFIG_HPP
#  include <boost/config.hpp>
#endif
#
#if defined(BOOST_HAS_PRAGMA_ONCE)
#  pragma once
#endif

#include <boost/move/utility_core.hpp>
#include <boost/move/adl_move_swap.hpp>
#include <boost/move/detail/iterator_traits.hpp>
#include <boost/move/algo/move.hpp>

namespace methcla_boost {
namespace movelib {

struct forward_t{};
struct backward_t{};
struct three_way_t{};
struct three_way_forward_t{};
struct four_way_t{};

struct move_op
{
   template <class SourceIt, class DestinationIt>
   inline void operator()(SourceIt source, DestinationIt dest)
   {  *dest = ::methcla_boost::move(*source);  }

   template <class SourceIt, class DestinationIt>
   inline DestinationIt operator()(forward_t, SourceIt first, SourceIt last, DestinationIt dest_begin)
   {  return ::methcla_boost::move(first, last, dest_begin);  }

   template <class SourceIt, class DestinationIt>
   inline DestinationIt operator()(backward_t, SourceIt first, SourceIt last, DestinationIt dest_last)
   {  return ::methcla_boost::move_backward(first, last, dest_last);  }

   template <class SourceIt, class DestinationIt1, class DestinationIt2>
   inline void operator()(three_way_t, SourceIt srcit, DestinationIt1 dest1it, DestinationIt2 dest2it)
   {
      *dest2it = methcla_boost::move(*dest1it);
      *dest1it = methcla_boost::move(*srcit);
   }

   template <class SourceIt, class DestinationIt1, class DestinationIt2>
   DestinationIt2 operator()(three_way_forward_t, SourceIt srcit, SourceIt srcitend, DestinationIt1 dest1it, DestinationIt2 dest2it)
   {
      //Destination2 range can overlap SourceIt range so avoid methcla_boost::move
      while(srcit != srcitend){
         this->operator()(three_way_t(), srcit++, dest1it++, dest2it++);
      }
      return dest2it;
   }

   template <class SourceIt, class DestinationIt1, class DestinationIt2, class DestinationIt3>
   inline void operator()(four_way_t, SourceIt srcit, DestinationIt1 dest1it, DestinationIt2 dest2it, DestinationIt3 dest3it)
   {
      *dest3it = methcla_boost::move(*dest2it);
      *dest2it = methcla_boost::move(*dest1it);
      *dest1it = methcla_boost::move(*srcit);
   }
};

struct swap_op
{
   template <class SourceIt, class DestinationIt>
   inline void operator()(SourceIt source, DestinationIt dest)
   {  methcla_boost::adl_move_swap(*dest, *source);  }

   template <class SourceIt, class DestinationIt>
   inline DestinationIt operator()(forward_t, SourceIt first, SourceIt last, DestinationIt dest_begin)
   {  return methcla_boost::adl_move_swap_ranges(first, last, dest_begin);  }

   template <class SourceIt, class DestinationIt>
   inline DestinationIt operator()(backward_t, SourceIt first, SourceIt last, DestinationIt dest_begin)
   {  return methcla_boost::adl_move_swap_ranges_backward(first, last, dest_begin);  }

   template <class SourceIt, class DestinationIt1, class DestinationIt2>
   inline void operator()(three_way_t, SourceIt srcit, DestinationIt1 dest1it, DestinationIt2 dest2it)
   {
      typename ::methcla_boost::movelib::iterator_traits<SourceIt>::value_type tmp(methcla_boost::move(*dest2it));
      *dest2it = methcla_boost::move(*dest1it);
      *dest1it = methcla_boost::move(*srcit);
      *srcit = methcla_boost::move(tmp);
   }

   template <class SourceIt, class DestinationIt1, class DestinationIt2>
   DestinationIt2 operator()(three_way_forward_t, SourceIt srcit, SourceIt srcitend, DestinationIt1 dest1it, DestinationIt2 dest2it)
   {
      while(srcit != srcitend){
         this->operator()(three_way_t(), srcit++, dest1it++, dest2it++);
      }
      return dest2it;
   }

   template <class SourceIt, class DestinationIt1, class DestinationIt2, class DestinationIt3>
   inline void operator()(four_way_t, SourceIt srcit, DestinationIt1 dest1it, DestinationIt2 dest2it, DestinationIt3 dest3it)
   {
      typename ::methcla_boost::movelib::iterator_traits<SourceIt>::value_type tmp(methcla_boost::move(*dest3it));
      *dest3it = methcla_boost::move(*dest2it);
      *dest2it = methcla_boost::move(*dest1it);
      *dest1it = methcla_boost::move(*srcit);
      *srcit = methcla_boost::move(tmp);
   }
};


}} //namespace methcla_boost::movelib

#endif   //BOOST_MOVE_ALGO_BASIC_OP
