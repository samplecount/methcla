/*
  LV2 Realtime Instantiate Extension
  Copyright 2012-2013 Stefan Kersten <stefan@samplecount.com>

  Permission to use, copy, modify, and/or distribute this software for any
  purpose with or without fee is hereby granted, provided that the above
  copyright notice and this permission notice appear in all copies.

  THIS SOFTWARE IS PROVIDED "AS IS" AND THE AUTHOR DISCLAIMS ALL WARRANTIES
  WITH REGARD TO THIS SOFTWARE INCLUDING ALL IMPLIED WARRANTIES OF
  MERCHANTABILITY AND FITNESS. IN NO EVENT SHALL THE AUTHOR BE LIABLE FOR
  ANY SPECIAL, DIRECT, INDIRECT, OR CONSEQUENTIAL DAMAGES OR ANY DAMAGES
  WHATSOEVER RESULTING FROM LOSS OF USE, DATA OR PROFITS, WHETHER IN AN
  ACTION OF CONTRACT, NEGLIGENCE OR OTHER TORTIOUS ACTION, ARISING OUT OF
  OR IN CONNECTION WITH THE USE OR PERFORMANCE OF THIS SOFTWARE.
*/

#ifndef LV2_RT_INSTANTIATE_H
#define LV2_RT_INSTANTIATE_H

#define LV2_RT_INSTANTIATE_URI        "http://methc.la/lv2/ext/rt-instantiate"
#define LV2_RT_INSTANTIATE__INTERFACE LV2_RT_INSTANTIATE_URI "#Interface"

/** @file
 * C header for the LV2 Instance Access extension
 * <http://lv2plug.in/ns/ext/instance-access>.
 *
 * This extension defines a method for (e.g.) plugin UIs to get a direct
 * handle to an LV2 plugin instance (LV2_Handle), if possible.
 *
 * To support this feature the host must pass an LV2_Feature struct to the
 * UI instantiate method with URI "http://lv2plug.in/ns/ext/instance-access"
 * and data pointed directly to the LV2_Handle of the plugin instance.
 */

typedef struct
{
    uint32_t (*instance_size)(const LV2_Descriptor *      descriptor);
    LV2_Handle (*instantiate)(const LV2_Descriptor *      descriptor,
                              double                      sample_rate,
                              const char *                bundle_path,
                              const LV2_Feature *const *  features,
                              void *                      location);
} LV2_RT_Instantiate_Interface;

inline static LV2_Handle
lv2_rt_instantiate_default_instantiate(
    const LV2_Descriptor *     descriptor,
    double                     sample_rate,
    const char *               bundle_path,
    const LV2_Feature *const * features)
{
    LV2_RT_Instantiate_Interface* rt = (LV2_RT_Instantiate_Interface*)descriptor->extension_data(LV2_RT_INSTANTIATE__INTERFACE);
    if (rt == NULL) return NULL;
    void* ptr = malloc(rt->instance_size(descriptor));
    if (ptr == NULL) return NULL;
    return rt->instantiate(descriptor, sample_rate, bundle_path, features, ptr);
}

#endif /* LV2_RT_INSTANTIATE_H */
