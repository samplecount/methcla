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

#ifndef METHCLA_PLATFORM_PEPPER_HPP_INCLUDED
#define METHCLA_PLATFORM_PEPPER_HPP_INCLUDED

#include "ppapi/cpp/instance_handle.h"

#include <methcla/common.h>
#include <methcla/engine.h>

METHCLA_EXPORT Methcla_AudioDriver* methcla_platform_pepper_audio_driver_new(
    const Methcla_AudioDriverOptions* options,
    const pp::InstanceHandle&         instance);

#endif // METHCLA_PLATFORM_PEPPER_HPP_INCLUDED
