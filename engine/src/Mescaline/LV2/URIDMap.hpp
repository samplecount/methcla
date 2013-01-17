#ifndef Mescaline_LV2_URIDMap_hpp_included
#define Mescaline_LV2_URIDMap_hpp_included

#include <Mescaline/Utility/Hash.hpp>

#include "lv2/lv2plug.in/ns/ext/urid/urid.h"

#include <boost/utility.hpp>
#include <unordered_map>

namespace Mescaline { namespace LV2 {

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
              , Mescaline::Utility::Hash::string_hash
              , Mescaline::Utility::Hash::string_equal_to >
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

#endif // Mescaline_LV2_URIDMap_hpp_included
