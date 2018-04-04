/*
 * Copyright (c) 2017-2018, NVIDIA CORPORATION.  All rights reserved.
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

vrs16_t
__gs_mod_16(vrs16_t x, vrs16_t y)
{
  return (__ZGVzN16vv__mth_i_vr4vr4(x, y, __mth_i_amod));
}

vrs16_t
__gs_mod_16m(vrs16_t x, vrs16_t y, vis16_t mask)
{
  return (__ZGVzM16vv__mth_i_vr4vr4(x, y, mask, __mth_i_amod));
}

vrd8_t
__gd_mod_8(vrd8_t x, vrd8_t y)
{
  return (__ZGVzN8vv__mth_i_vr8vr8(x, y, __mth_i_dmod));
}

vrd8_t
__gd_mod_8m(vrd8_t x, vrd8_t y, vid8_t mask)
{
  return (__ZGVzM8vv__mth_i_vr8vr8(x, y, mask, __mth_i_dmod));
}
