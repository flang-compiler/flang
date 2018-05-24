
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

vrd2_t
__gd_powi1_2(vrd2_t x, int32_t iy)
{
  return(__ZGVxN2v__mth_i_vr8si4(x, iy, __mth_i_dpowi));
}

vrd2_t
__gd_powi1_2m(vrd2_t x, int32_t iy, vid2_t mask)
{
  return(__ZGVxM2v__mth_i_vr8si4(x, iy, mask, __mth_i_dpowi));
}

/*
 * __gd_powi_2 and __gd_powi_2m should technically be defined as:
 * __gd_powi_2(vrd2_t x, vis2_t iy)
 * __gd_powi_2m(vrd2_t x, vis2_t iy, vid2_t mask)
 *
 * But the POWER architectures needs the 32-bit integer vectors to
 * be the full 128-bits of a vector register.
 */

vrd2_t
__gd_powi_2(vrd2_t x, vis4_t iy)
{
  return(__ZGVxN2vv__mth_i_vr8vi4(x, iy, __mth_i_dpowi));
}

vrd2_t
__gd_powi_2m(vrd2_t x, vis4_t iy, vid2_t mask)
{
  return(__ZGVxM2vv__mth_i_vr8vi4(x, iy, mask, __mth_i_dpowi));
}

vrd2_t
__gd_powk1_2(vrd2_t x, long long iy)
{
  return(__ZGVxN2v__mth_i_vr8si8(x, iy, __mth_i_dpowk));
}

vrd2_t
__gd_powk1_2m(vrd2_t x, long long iy, vid2_t mask)
{
  return(__ZGVxM2v__mth_i_vr8si8(x, iy, mask, __mth_i_dpowk));
}

vrd2_t
__gd_powk_2(vrd2_t x, vid2_t iy)
{
  return(__ZGVxN2vv__mth_i_vr8vi8(x, iy, __mth_i_dpowk));
}

vrd2_t
__gd_powk_2m(vrd2_t x, vid2_t iy, vid2_t mask)
{
  return(__ZGVxM2vv__mth_i_vr8vi8(x, iy, mask, __mth_i_dpowk));
}

vcs1_t
__gc_powi_1(vcs1_t x, int iy)
{
  return(__ZGVxN1v__mth_i_vc4si4(x, iy, __mth_i_cpowi_c99));
}

vcs1_t
__gc_powk_1(vcs1_t x, long long iy)
{
  return(__ZGVxN1v__mth_i_vc4si8(x, iy, __mth_i_cpowk_c99));
}

double complex
__gz_powi_1(double complex x, int iy)
{
  return(__mth_i_cdpowi_c99(x, iy));
}

double complex
__gz_powk_1(double complex x, long long iy)
{
  return(__mth_i_cdpowk_c99(x, iy));
}

vcd1_t
__gz_powi_1v(vcd1_t x, int iy)
{
  return(__ZGVxN1v__mth_i_vc8si4(x, iy, __mth_i_cdpowi_c99));
}

vcd1_t
__gz_powk_1v(vcd1_t x, long long iy)
{
  return(__ZGVxN1v__mth_i_vc8si8(x, iy, __mth_i_cdpowk_c99));
}
