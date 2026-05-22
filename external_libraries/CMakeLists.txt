# tlsf — two-level segregated fit allocator; OBJECT so its object files are
# merged directly into the methcla target and not exposed to users.
add_library(tlsf OBJECT tlsf/tlsf.c)
target_include_directories(tlsf PUBLIC tlsf)

# tinydir — header-only portable directory listing
add_library(tinydir INTERFACE)
target_include_directories(tinydir INTERFACE ${CMAKE_CURRENT_SOURCE_DIR}/tinydir)

# boost_headers — vendored Boost subset (lockfree, heap, and transitive deps)
add_library(boost_headers INTERFACE)
target_include_directories(boost_headers SYSTEM INTERFACE
    ${CMAKE_CURRENT_SOURCE_DIR}/boost
)
