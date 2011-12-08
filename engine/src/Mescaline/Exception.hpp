#ifndef MESCALINE_EXCEPTION_HPP_INCLUDED
#define MESCALINE_EXCEPTION_HPP_INCLUDED

#include <boost/exception/all.hpp>
#include <string>
#include <stdexcept>

namespace Mescaline {

struct Exception : virtual std::exception, virtual boost::exception { };
struct MemoryAllocationFailure : virtual Exception { };
struct InvalidInput : virtual Exception { };

typedef boost::error_info<struct ErrorInfoStringTag, std::string> ErrorInfoString;

};

#endif // MESCALINE_EXCEPTION_HPP_INCLUDED