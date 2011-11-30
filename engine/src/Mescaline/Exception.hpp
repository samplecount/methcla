#ifndef MESCALINE_EXCEPTION_HPP_INCLUDED
#define MESCALINE_EXCEPTION_HPP_INCLUDED

#include <stdexcept>
#include <boost/exception/all.hpp>

namespace Mescaline {
struct Exception : virtual std::exception, virtual boost::exception { };
struct MemoryAllocationFailure : virtual Exception { };
};

#endif // MESCALINE_EXCEPTION_HPP_INCLUDED