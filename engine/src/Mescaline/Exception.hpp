#ifndef MESCALINE_EXCEPTION_HPP_INCLUDED
#define MESCALINE_EXCEPTION_HPP_INCLUDED

#include <boost/exception/all.hpp>
#include <string>
#include <stdexcept>

namespace Mescaline {

struct Exception : virtual std::exception, virtual boost::exception
{
    typedef boost::error_info<struct tag_StringInfo,std::string> StringInfo;
};

struct MemoryAllocationFailure : virtual Exception { };
struct InvalidInput : virtual Exception { };

};

#endif // MESCALINE_EXCEPTION_HPP_INCLUDED