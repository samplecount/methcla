/*
 * Copyright 2017 James E. King, III
 *
 * Distributed under the Boost Software License, Version 1.0.
 * See http://www.boost.org/LICENSE_1_0.txt
 */

#ifndef BOOST_WINAPI_BCRYPT_HPP_INCLUDED_
#define BOOST_WINAPI_BCRYPT_HPP_INCLUDED_

#include <boost/winapi/basic_types.hpp>

#ifdef BOOST_HAS_PRAGMA_ONCE
#pragma once
#endif

#if BOOST_USE_WINAPI_VERSION >= BOOST_WINAPI_VERSION_WIN6

#if BOOST_WINAPI_PARTITION_APP_SYSTEM

#if defined(BOOST_USE_WINDOWS_H)
#include <bcrypt.h>
#endif

#include <boost/winapi/detail/header.hpp>

#if defined(BOOST_USE_WINDOWS_H)

namespace methcla_boost { namespace winapi {
typedef ::BCRYPT_ALG_HANDLE BCRYPT_ALG_HANDLE_;
}}

#else // defined(BOOST_USE_WINDOWS_H)

namespace methcla_boost { namespace winapi {
typedef PVOID_ BCRYPT_ALG_HANDLE_;
}}

extern "C" {

methcla_boost::winapi::NTSTATUS_ BOOST_WINAPI_WINAPI_CC
BCryptCloseAlgorithmProvider(
    methcla_boost::winapi::BCRYPT_ALG_HANDLE_ hAlgorithm,
    methcla_boost::winapi::ULONG_             dwFlags
);

methcla_boost::winapi::NTSTATUS_ BOOST_WINAPI_WINAPI_CC
BCryptGenRandom(
    methcla_boost::winapi::BCRYPT_ALG_HANDLE_ hAlgorithm,
    methcla_boost::winapi::PUCHAR_            pbBuffer,
    methcla_boost::winapi::ULONG_             cbBuffer,
    methcla_boost::winapi::ULONG_             dwFlags
);

methcla_boost::winapi::NTSTATUS_ BOOST_WINAPI_WINAPI_CC
BCryptOpenAlgorithmProvider(
    methcla_boost::winapi::BCRYPT_ALG_HANDLE_ *phAlgorithm,
    methcla_boost::winapi::LPCWSTR_           pszAlgId,
    methcla_boost::winapi::LPCWSTR_           pszImplementation,
    methcla_boost::winapi::DWORD_             dwFlags
);

} // extern "C"

#endif // defined(BOOST_USE_WINDOWS_H)

namespace methcla_boost {
namespace winapi {

#if defined(BOOST_USE_WINDOWS_H)
const WCHAR_ BCRYPT_RNG_ALGORITHM_[] = BCRYPT_RNG_ALGORITHM;
#else
const WCHAR_ BCRYPT_RNG_ALGORITHM_[] = L"RNG";
#endif

using ::BCryptCloseAlgorithmProvider;
using ::BCryptGenRandom;
using ::BCryptOpenAlgorithmProvider;

} // winapi
} // boost

#include <boost/winapi/detail/footer.hpp>

#endif // BOOST_WINAPI_PARTITION_APP_SYSTEM

#endif // BOOST_USE_WINAPI_VERSION >= BOOST_WINAPI_VERSION_WIN6

#endif // BOOST_WINAPI_BCRYPT_HPP_INCLUDED_
