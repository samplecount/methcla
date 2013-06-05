LOCAL_PATH := $(call my-dir)

include $(CLEAR_VARS)
LOCAL_MODULE := methcla
LOCAL_SRC_FILES := $(TARGET_ARCH_ABI)/libmethcla.a
LOCAL_EXPORT_C_INCLUDES := $(LOCAL_PATH)/../../include $(LOCAL_PATH)/../../plugins
include $(PREBUILT_STATIC_LIBRARY)

