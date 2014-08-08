// Copyright 2012-2013 Samplecount S.L.
// 
// Licensed under the Apache License, Version 2.0 (the "License");
// you may not use this file except in compliance with the License.
// You may obtain a copy of the License at
// 
//     http://www.apache.org/licenses/LICENSE-2.0
// 
// Unless required by applicable law or agreed to in writing, software
// distributed under the License is distributed on an "AS IS" BASIS,
// WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
// See the License for the specific language governing permissions and
// limitations under the License.

#ifndef METHCLA_UTILITY_MACROS_H_INCLUDED
#define METHCLA_UTILITY_MACROS_H_INCLUDED

#if defined(__GNUC__)
# define METHCLA_WITHOUT_WARNINGS_BEGIN \
    _Pragma("GCC diagnostic push") \
    _Pragma("GCC diagnostic ignored \"-Wunused-parameter\"") \
    _Pragma("GCC diagnostic ignored \"-Wunused-private-field\"")
# define METHCLA_WITHOUT_WARNINGS_END \
    _Pragma("GCC diagnostic pop")
#else
# define METHCLA_WITHOUT_WARNINGS_BEGIN
# define METHCLA_WITHOUT_WARNINGS_END
#endif

#endif // METHCLA_UTILITY_MACROS_H_INCLUDED
