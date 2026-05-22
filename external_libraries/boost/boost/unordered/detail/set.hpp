
// Copyright (C) 2005-2016 Daniel James
// Copyright (C) 2022 Christian Mazakas
// Distributed under the Boost Software License, Version 1.0. (See accompanying
// file LICENSE_1_0.txt or copy at http://www.boost.org/LICENSE_1_0.txt)

#include <boost/unordered/detail/implementation.hpp>
#include <boost/unordered/unordered_set_fwd.hpp>

namespace methcla_boost {
  namespace unordered {
    namespace detail {
      template <typename A, typename T, typename H, typename P> struct set
      {
        typedef methcla_boost::unordered::detail::set<A, T, H, P> types;

        typedef T value_type;
        typedef H hasher;
        typedef P key_equal;
        typedef T const const_key_type;

        typedef
          typename ::methcla_boost::unordered::detail::rebind_wrap<A, value_type>::type
            value_allocator;
        typedef methcla_boost::unordered::detail::allocator_traits<value_allocator>
          value_allocator_traits;

        typedef methcla_boost::unordered::detail::table<types> table;
        typedef methcla_boost::unordered::detail::set_extractor<value_type> extractor;

        typedef typename methcla_boost::allocator_void_pointer<value_allocator>::type
          void_pointer;

        typedef methcla_boost::unordered::node_handle_set<
          node<value_type, void_pointer>, T, A>
          node_type;

        typedef typename table::c_iterator iterator;
        typedef methcla_boost::unordered::insert_return_type_set<iterator, node_type>
          insert_return_type;
      };
    }
  }
}
