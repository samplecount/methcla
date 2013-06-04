# Copyright (C) 2013 Samplecount S.L.
#
# Licensed under the Apache License, Version 2.0 (the "License");
# you may not use this file except in compliance with the License.
# You may obtain a copy of the License at
#
#      http://www.apache.org/licenses/LICENSE-2.0
#
# Unless required by applicable law or agreed to in writing, software
# distributed under the License is distributed on an "AS IS" BASIS,
# WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
# See the License for the specific language governing permissions and
# limitations under the License.

LOCAL_PATH := $(call my-dir)
METHCLA_SRC_DIR := ../../..

include $(CLEAR_VARS)
LOCAL_MODULE := methcla
LOCAL_SRC_FILES := ../$(METHCLA_SRC_DIR)/build/debug/android/armv5/libmethcla.a
LOCAL_EXPORT_C_INCLUDES := $(METHCLA_SRC_DIR)/include $(METHCLA_SRC_DIR)/plugins
include $(PREBUILT_STATIC_LIBRARY)

include $(CLEAR_VARS)
LOCAL_MODULE    := thaddeus
LOCAL_SRC_FILES := main.cpp synth.cpp
LOCAL_CPPFLAGS  := -std=c++11
LOCAL_CPP_FEATURES := rtti exceptions
LOCAL_C_INCLUDES  := $(METHCLA_SRC_DIR)/external_libraries/boost $(METHCLA_SRC_DIR)/external_libraries/oscpp
LOCAL_LDLIBS    := -llog -landroid -lEGL -lGLESv1_CM -lOpenSLES
LOCAL_STATIC_LIBRARIES := android_native_app_glue methcla
LOCAL_DISABLE_NO_EXECUTE := true
include $(BUILD_SHARED_LIBRARY)

$(call import-module,android/native_app_glue)
