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
__pz_powi1_16(vrs16_t x, int32_t iy)
{
  return(__ZGVzN16v__mth_i_vr4si4(x, iy, __pmth_i_rpowi));
}

vrs16_t
__pz_powi1_16m(vrs16_t x, int32_t iy, vis16_t mask)
{
  return(__ZGVzM16v__mth_i_vr4si4(x, iy, mask, __pmth_i_rpowi));
}

vrs16_t
__pz_powi_16(vrs16_t x, vis16_t iy)
{
  return(__ZGVzN16vv__mth_i_vr4vi4(x, iy, __pmth_i_rpowi));
}

vrs16_t
__pz_powi_16m(vrs16_t x, vis16_t iy, vis16_t mask)
{
  return(__ZGVzM16vv__mth_i_vr4vi4(x, iy, mask, __pmth_i_rpowi));
}

vrs16_t
__pz_powk1_16(vrs16_t x, long long iy)
{
  return(__ZGVzN16v__mth_i_vr4si8(x, iy, __pmth_i_rpowk));
}

vrs16_t
__pz_powk1_16m(vrs16_t x, long long iy, vis16_t mask)
{
  return(__ZGVzM16v__mth_i_vr4si8(x, iy, mask, __pmth_i_rpowk));
}

vrs16_t
__pz_powk_16(vrs16_t x, vid8_t iyu, vid8_t iyl)
{
  return(__ZGVzN16vv__mth_i_vr4vi8(x, iyu, iyl, __pmth_i_rpowk));
}

vrs16_t
__pz_powk_16m(vrs16_t x, vid8_t iyu, vid8_t iyl, vis16_t mask)
{
  return(__ZGVzM16vv__mth_i_vr4vi8(x, iyu, iyl, mask, __pmth_i_rpowk));
}

vrd8_t
__pz_powi1_8(vrd8_t x, int32_t iy)
{
  return(__ZGVzN8v__mth_i_vr8si4(x, iy, __pmth_i_dpowi));
}

vrd8_t
__pz_powi1_8m(vrd8_t x, int32_t iy, vid8_t mask)
{
  return(__ZGVzM8v__mth_i_vr8si4(x, iy, mask, __pmth_i_dpowi));
}

vrd8_t
__pz_powi_8(vrd8_t x, vis8_t iy)
{
  return(__ZGVzN8vv__mth_i_vr8vi4(x, iy, __pmth_i_dpowi));
}

vrd8_t
__pz_powi_8m(vrd8_t x, vis8_t iy, vid8_t mask)
{
  return(__ZGVzM8vv__mth_i_vr8vi4(x, iy, mask, __pmth_i_dpowi));
}

vrd8_t
__pz_powk1_8(vrd8_t x, long long iy)
{
  return(__ZGVzN8v__mth_i_vr8si8(x, iy, __pmth_i_dpowk));
}

vrd8_t
__pz_powk1_8m(vrd8_t x, long long iy, vid8_t mask)
{
  return(__ZGVzM8v__mth_i_vr8si8(x, iy, mask, __pmth_i_dpowk));
}

vrd8_t
__pz_powk_8(vrd8_t x, vid8_t iy)
{
  return(__ZGVzN8vv__mth_i_vr8vi8(x, iy, __pmth_i_dpowk));
}

vrd8_t
__pz_powk_8m(vrd8_t x, vid8_t iy, vid8_t mask)
{
  return(__ZGVzM8vv__mth_i_vr8vi8(x, iy, mask, __pmth_i_dpowk));
}
