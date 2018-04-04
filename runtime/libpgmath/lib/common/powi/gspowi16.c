
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
__gs_powi1_16(vrs16_t x, int32_t iy)
{
  return(__ZGVzN16v__mth_i_vr4si4(x, iy, __mth_i_rpowi));
}

vrs16_t
__gs_powi1_16m(vrs16_t x, int32_t iy, vis16_t mask)
{
  return(__ZGVzM16v__mth_i_vr4si4(x, iy, mask, __mth_i_rpowi));
}

vrs16_t
__gs_powi_16(vrs16_t x, vis16_t iy)
{
  return(__ZGVzN16vv__mth_i_vr4vi4(x, iy, __mth_i_rpowi));
}

vrs16_t
__gs_powi_16m(vrs16_t x, vis16_t iy, vis16_t mask)
{
  return(__ZGVzM16vv__mth_i_vr4vi4(x, iy, mask, __mth_i_rpowi));
}

vrs16_t
__gs_powk1_16(vrs16_t x, long long iy)
{
  return(__ZGVzN16v__mth_i_vr4si8(x, iy, __mth_i_rpowk));
}

vrs16_t
__gs_powk1_16m(vrs16_t x, long long iy, vis16_t mask)
{
  return(__ZGVzM16v__mth_i_vr4si8(x, iy, mask, __mth_i_rpowk));
}

vrs16_t
__gs_powk_16(vrs16_t x, vid8_t iyl, vid8_t iyu)
{
  return(__ZGVzN16vv__mth_i_vr4vi8(x, iyl, iyu, __mth_i_rpowk));
}

vrs16_t
__gs_powk_16m(vrs16_t x, vid8_t iyl, vid8_t iyu, vis16_t mask)
{
  return(__ZGVzM16vv__mth_i_vr4vi8(x, iyl, iyu, mask, __mth_i_rpowk));
}
