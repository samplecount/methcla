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

#include "Methcla/Utility/Macros.h"

METHCLA_WITHOUT_WARNINGS_BEGIN
#define CATCH_CONFIG_RUNNER
#include <catch.hpp>
METHCLA_WITHOUT_WARNINGS_END

#include "catch_callback_reporter.hpp"
#include "ppapi/cpp/instance.h"
#include "ppapi/cpp/module.h"
#include "ppapi/cpp/var.h"

#include <string>

class MethclaTestInstance : public pp::Instance
{
public:
    explicit MethclaTestInstance(PP_Instance instance)
    : pp::Instance(instance)
    {}

private:
    virtual void HandleMessage(const pp::Var& var_message)
    {
        Catch::registerCallbackReporter("callback",
                                        [this](const std::string& str) {
                                            this->PostMessage(pp::Var(str));
                                        });

        Catch::Session session; // There must be exactly once instance
        session.configData().reporterName = "callback";
        session.run();
    }
};

class MethclaTestModule : public pp::Module
{
public:
    virtual ~MethclaTestModule() {}

    virtual pp::Instance* CreateInstance(PP_Instance instance)
    {
        return new MethclaTestInstance(instance);
    }
};

namespace pp {
    Module* CreateModule() { return new MethclaTestModule(); }
} // namespace pp
