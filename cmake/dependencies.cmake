include(FetchContent)

# oscpp — header-only OSC library, part of methcla's public API.
# OSCPP_BUILD_TESTS defaults to PROJECT_IS_TOP_LEVEL, so tests are off when
# consumed via FetchContent.
FetchContent_Declare(
  oscpp
  GIT_REPOSITORY https://github.com/kaoskorobase/oscpp.git
  GIT_TAG a62fe7690ce3563c997d7c9915d8cb54ff8f79b0
  FIND_PACKAGE_ARGS CONFIG)
FetchContent_MakeAvailable(oscpp)

if(METHCLA_BUILD_TESTS)
  # googletest — test-only dependency
  FetchContent_Declare(
    googletest
    GIT_REPOSITORY https://github.com/google/googletest.git
    GIT_TAG v1.14.0
    FIND_PACKAGE_ARGS NAMES GTest)
  set(INSTALL_GTEST
      OFF
      CACHE BOOL "" FORCE)
  FetchContent_MakeAvailable(googletest)
endif()
