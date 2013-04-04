/*
    Copyright 2012-2013 Samplecount S.L.

    Licensed under the Apache License, Version 2.0 (the "License");
    you may not use this file except in compliance with the License.
    You may obtain a copy of the License at

        http://www.apache.org/licenses/LICENSE-2.0

    Unless required by applicable law or agreed to in writing, software
    distributed under the License is distributed on an "AS IS" BASIS,
    WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
    See the License for the specific language governing permissions and
    limitations under the License.
*/

#ifndef METHCLA_LV2_PLUGINS_SINE_H_INCLUDED
#define METHCLA_LV2_PLUGINS_SINE_H_INCLUDED

#include <methcla/common.h>
#include <methcla/engine.h>
#include "lv2/lv2plug.in/ns/lv2core/lv2.h"
METHCLA_EXPORT const LV2_Lib_Descriptor* methcla_lv2_plugins_sine_lv2_lib_descriptor(const char*, const LV2_Feature *const *);
#define METHCLA_LV2_PLUGINS_SINE_URI "http://methc.la/lv2/plugins/sine"
#define METHCLA_LV2_PLUGINS_SINE_LIB { METHCLA_LV2_PLUGINS_SINE_URI, (Methcla_Library_Function)methcla_lv2_plugins_sine_lv2_lib_descriptor }

#endif /* METHCLA_LV2_PLUGINS_SINE_H_INCLUDED */
