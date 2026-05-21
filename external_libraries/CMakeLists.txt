# tlsf — two-file C allocator
add_library(tlsf STATIC tlsf/tlsf.c)
target_include_directories(tlsf PUBLIC tlsf)

# oscpp — header-only OSC library
add_library(oscpp INTERFACE)
target_include_directories(oscpp SYSTEM INTERFACE
    ${CMAKE_CURRENT_SOURCE_DIR}/oscpp/include
)

# boost_headers — vendored Boost subset (lockfree, heap, and transitive deps)
add_library(boost_headers INTERFACE)
target_include_directories(boost_headers SYSTEM INTERFACE
    ${CMAKE_CURRENT_SOURCE_DIR}/boost
)

# tinydir — header-only directory traversal
add_library(tinydir INTERFACE)
target_include_directories(tinydir INTERFACE
    ${CMAKE_CURRENT_SOURCE_DIR}/tinydir
)
