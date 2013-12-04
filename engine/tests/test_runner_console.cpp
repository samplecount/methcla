// Copyright 2013 Samplecount S.L.
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

#include "Methcla/Utility/Macros.h"

METHCLA_WITHOUT_WARNINGS_BEGIN
# define CATCH_CONFIG_RUNNER
# include <catch.hpp>
METHCLA_WITHOUT_WARNINGS_END

int main(int argc, char* const argv[])
{
    return Catch::Session().run(argc, argv);
}
