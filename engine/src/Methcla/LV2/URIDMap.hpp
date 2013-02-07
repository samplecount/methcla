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

#ifndef METHCLA_LV2_URIDMAP_HPP_INCLUDED
#define METHCLA_LV2_URIDMAP_HPP_INCLUDED

#include <Methcla/Utility/Hash.hpp>

#include "lv2/lv2plug.in/ns/ext/urid/urid.h"

#include <boost/utility.hpp>
#include <unordered_map>

namespace Methcla { namespace LV2 {

class URIDMap : boost::noncopyable
{
public:
    URIDMap();

    LV2_URID map(const char* uri);
    const char* unmap(LV2_URID urid) const;

    LV2_URID_Map* lv2Map();
    LV2_URID_Unmap* lv2Unmap();

private:
    LV2_URID insert(const char* uri);

private:
    typedef std::unordered_map<
                const char*
              , LV2_URID
              , Methcla::Utility::Hash::string_hash
              , Methcla::Utility::Hash::string_equal_to >
            UriToId;
    typedef std::unordered_map<
                LV2_URID
              , const char* >
            IdToUri;
    
    UriToId 		m_uriToId;
    IdToUri 		m_idToUri;
    LV2_URID_Map    m_lv2Map;
    LV2_URID_Unmap  m_lv2Unmap;
};

}; };

#endif // METHCLA_LV2_URIDMAP_HPP_INCLUDED
