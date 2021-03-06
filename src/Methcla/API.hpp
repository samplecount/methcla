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

#ifndef METHCLA_API_HPP_INCLUDED
#define METHCLA_API_HPP_INCLUDED

#include "Methcla/Audio/Engine.hpp"
#include "Methcla/Audio/IO/Driver.hpp"

#include <methcla/engine.h>

#include <functional>

namespace Methcla { namespace API {
    Methcla::Audio::IO::Driver::Options
    convertOptions(const Methcla_AudioDriverOptions* options);
    Methcla::Audio::Environment::Options
    convertOptions(const Methcla_EngineOptions* options);

    Methcla_AudioDriver* wrapAudioDriver(Methcla::Audio::IO::Driver* driver);
    Methcla::Audio::IO::Driver* getDriver(Methcla_Engine* engine);
}} // namespace Methcla::API

#endif // METHCLA_API_HPP_INCLUDED