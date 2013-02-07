/*
  LV2 Realtime Instantiate Extension
  Copyright 2012 Stefan Kersten <http://puesnada.es>

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
    void (*initialize)(LV2_Descriptor *                   descriptor,
                       const char *                       bundle_path,
                       const LV2_Feature *const *         features);
    void (*release)(LV2_Descriptor *                      descriptor);

    uint32_t (*instance_size)(const LV2_Descriptor *      descriptor);
    uint32_t (*instance_alignment)(const LV2_Descriptor * descriptor);

    LV2_Handle (*instantiate)(const LV2_Descriptor *      descriptor,
                              void *                      location,
                              double                      sample_rate);
} LV2_RT_Instantiate_Interface;


#endif /* LV2_RT_INSTANTIATE_H */
