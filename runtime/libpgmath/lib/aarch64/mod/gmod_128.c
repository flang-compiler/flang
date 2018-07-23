/*
 * Copyright (c) 2018, NVIDIA CORPORATION.  All rights reserved.
 *
 * Licensed under the Apache License, Version 2.0 (the "License");
 * you may not use this file except in compliance with the License.
 * You may obtain a copy of the License at
 *
 *     http://www.apache.org/licenses/LICENSE-2.0
 *
 * Unless required by applicable law or agreed to in writing, software
 * distributed under the License is distributed on an "AS IS" BASIS,
 * WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 * See the License for the specific language governing permissions and
 * limitations under the License.
 *
 */

#include "mth_intrinsics.h"

#if defined(TARGET_LINUX_ARM64)
vrs4_t
__gs_mod_4(vrs4_t x, vrs4_t y)
{
  return (__ZGVxN4vv__mth_i_vr4vr4(x, y, __mth_i_amod));
}

vrs4_t
__gs_mod_4m(vrs4_t x, vrs4_t y, vis4_t mask)
{
  return (__ZGVxM4vv__mth_i_vr4vr4(x, y, mask, __mth_i_amod));
}

vrd2_t
__gd_mod_2(vrd2_t x, vrd2_t y)
{
  return (__ZGVxN2vv__mth_i_vr8vr8(x, y, __mth_i_dmod));
}

vrd2_t
__gd_mod_2m(vrd2_t x, vrd2_t y, vid2_t mask)
{
  return (__ZGVxM2vv__mth_i_vr8vr8(x, y, mask, __mth_i_dmod));
}
#endif

