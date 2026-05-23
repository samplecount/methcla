add_executable(dumposcfile dumposcfile.cpp)
target_link_libraries(dumposcfile PRIVATE oscpp::oscpp)
methcla_target_warnings(dumposcfile)
