/*
 * Copyright (c) 2016-2018, NVIDIA CORPORATION.  All rights reserved.
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

vrs8_t
__ZGVyN8v__mth_i_vr4(vrs8_t x, float func(float))
{
  int i;
  vrs8_t r;
  for (i = 0; i < 8; i++) {
    r[i] = func(x[i]);
  }
  return r;
}

vrs8_t
__ZGVyM8v__mth_i_vr4(vrs8_t x, vis8_t mask, float func(float))
{
  int i;
  vrs8_t r;
  for (i = 0; i < 8; i++) {
    if (mask[i])
      r[i] = func(x[i]);
  }
  return r;
}

vrs8_t
__ZGVyN8vv__mth_i_vr4vr4(vrs8_t x, vrs8_t y, float func(float, float))
{
  int i;
  vrs8_t r;
  for (i = 0; i < 8; i++) {
    r[i] = func(x[i], y[i]);
  }
  return r;
}

vrs8_t
__ZGVyM8vv__mth_i_vr4vr4(vrs8_t x, vrs8_t y, vis8_t mask, float func(float, float))
{
  int i;
  vrs8_t r;
  for (i = 0; i < 8; i++) {
    if (mask[i])
      r[i] = func(x[i], y[i]);
  }
  return r;
}

vrd4_t
__ZGVyN4v__mth_i_vr8(vrd4_t x, double func(double))
{
  int i;
  vrd4_t r;
  for (i = 0; i < 4; i++) {
    r[i] = func(x[i]);
  }
  return r;
}

vrd4_t
__ZGVyM4v__mth_i_vr8(vrd4_t x, vid4_t mask, double func(double))
{
  int i;
  vrd4_t r;
  for (i = 0; i < 4; i++) {
    if (mask[i])
      r[i] = func(x[i]);
  }
  return r;
}

vrd4_t
__ZGVyN4vv__mth_i_vr8vr8(vrd4_t x, vrd4_t y, double func(double, double))
{
  int i;
  vrd4_t r;
  for (i = 0; i < 4; i++) {
    r[i] = func(x[i], y[i]);
  }
  return r;
}

vrd4_t
__ZGVyM4vv__mth_i_vr8vr8(vrd4_t x, vrd4_t y, vid4_t mask, double func(double, double))
{
  int i;
  vrd4_t r;
  for (i = 0; i < 4; i++) {
    if (mask[i])
      r[i] = func(x[i], y[i]);
  }
  return r;
}

vrs8_t
__ZGVyN8v__mth_i_vr4si4(vrs8_t x, int32_t iy, float func(float, int32_t))
{
  int i;
  vrs8_t r;
  for (i = 0 ; i < 8 ; i++) {
    r[i] = func(x[i], iy);
  }
  return r;
}

vrs8_t
__ZGVyM8v__mth_i_vr4si4(vrs8_t x, int32_t iy, vis8_t mask, float func(float, int32_t))
{
  int i;
  vrs8_t r;
  for (i = 0 ; i < 8 ; i++) {
    if (mask[i])
      r[i] = func(x[i], iy);
  }
  return r;
}

vrs8_t
__ZGVyN8vv__mth_i_vr4vi4(vrs8_t x, vis8_t iy, float func(float, int32_t))
{
  int i;
  vrs8_t r;
  for (i = 0 ; i < 8 ; i++) {
    r[i] = func(x[i], iy[i]);
  }
  return r;
}

vrs8_t
__ZGVyM8vv__mth_i_vr4vi4(vrs8_t x, vis8_t iy, vis8_t mask, float func(float, int32_t))
{
  int i;
  vrs8_t r;
  for (i = 0 ; i < 8 ; i++) {
    if (mask[i])
      r[i] = func(x[i], iy[i]);
  }
  return r;
}

vrs8_t
__ZGVyN8v__mth_i_vr4si8(vrs8_t x, long long iy, float func(float, long long))
{
  int i;
  vrs8_t r;
  for (i = 0 ; i < 8 ; i++) {
    r[i] = func(x[i], iy);
  }
  return r;
}

vrs8_t
__ZGVyM8v__mth_i_vr4si8(vrs8_t x, long long iy, vis8_t mask, float func(float, long long))
{
  int i;
  vrs8_t r;
  for (i = 0 ; i < 8 ; i++) {
    if (mask[i])
      r[i] = func(x[i], iy);
  }
  return r;
}

vrs8_t
__ZGVyN8vv__mth_i_vr4vi8(vrs8_t x, vid4_t iyu, vid4_t iyl, float func(float, long long))
{
  int i;
  vrs8_t r;
  for (i = 0 ; i < 4 ; i++) {
    r[i] = func(x[i], iyu[i]);
  }
  for (i = 4 ; i < 8 ; i++) {
    r[i] = func(x[i], iyl[i-4]);
  }
  return r;
}

vrs8_t
__ZGVyM8vv__mth_i_vr4vi8(vrs8_t x, vid4_t iyu, vid4_t iyl, vis8_t mask, float func(float, long long))
{
  int i;
  vrs8_t r;
  for (i = 0 ; i < 4 ; i++) {
    if (mask[i])
      r[i] = func(x[i], iyu[i]);
  }
  for (i = 4 ; i < 8 ; i++) {
    if (mask[i])
      r[i] = func(x[i], iyl[i-4]);
  }
  return r;
}

vrd4_t
__ZGVyN4v__mth_i_vr8si4(vrd4_t x, int32_t iy, double func(double, int32_t))
{
  int i;
  vrd4_t r;
  for (i = 0 ; i < 4 ; i++) {
    r[i] = func(x[i], iy);
  }
  return r;
}

vrd4_t
__ZGVyM4v__mth_i_vr8si4(vrd4_t x, int32_t iy, vid4_t mask, double func(double, int32_t))
{
  int i;
  vrd4_t r;
  for (i = 0 ; i < 4 ; i++) {
    if (mask[i])
      r[i] = func(x[i], iy);
  }
  return r;
}

vrd4_t
__ZGVyN4vv__mth_i_vr8vi4(vrd4_t x, vis4_t iy, double func(double, int32_t))
{
  int i;
  vrd4_t r;
  for (i = 0 ; i < 4 ; i++) {
    r[i] = func(x[i], iy[i]);
  }
  return r;
}

vrd4_t
__ZGVyM4vv__mth_i_vr8vi4(vrd4_t x, vis4_t iy, vid4_t mask, double func(double, int32_t))
{
  int i;
  vrd4_t r;
  for (i = 0 ; i < 4 ; i++) {
    if (mask[i])
      r[i] = func(x[i], iy[i]);
  }
  return r;
}

vrd4_t
__ZGVyN4v__mth_i_vr8si8(vrd4_t x, long long iy, double func(double, long long))
{
  int i;
  vrd4_t r;
  for (i = 0 ; i < 4 ; i++) {
    r[i] = func(x[i], iy);
  }
  return r;
}

vrd4_t
__ZGVyM4v__mth_i_vr8si8(vrd4_t x, long long iy, vid4_t mask, double func(double, long long))
{
  int i;
  vrd4_t r;
  for (i = 0 ; i < 4 ; i++) {
    if (mask[i])
      r[i] = func(x[i], iy);
  }
  return r;
}

vrd4_t
__ZGVyN4vv__mth_i_vr8vi8(vrd4_t x, vid4_t iy, double func(double, long long))
{
  int i;
  vrd4_t r;
  for (i = 0 ; i < 4 ; i++) {
    r[i] = func(x[i], iy[i]);
  }
  return r;
}

vrd4_t
__ZGVyM4vv__mth_i_vr8vi8(vrd4_t x, vid4_t iy, vid4_t mask, double func(double, long long))
{
  int i;
  vrd4_t r;
  for (i = 0 ; i < 4 ; i++) {
    if (mask[i])
      r[i] = func(x[i], iy[i]);
  }
  return r;
}

vcs4_t
__ZGVyN4v__mth_i_vc4(vcs4_t x, float complex func(float complex))
{
  int i;
  float complex tx[4];
  *(vcs4_t *)&tx = x;
  for (i = 0 ; i < 4 ; i++) {
    tx[i] = func(tx[i]);
  }
  return *(vcs4_t *)&tx;
}

vcs4_t
__ZGVyN4vv__mth_i_vc4vc4(vcs4_t x, vcs4_t y, float complex func(float complex, float complex))
{
  int i;
  float complex tx[4];
  float complex ty[4];
  *(vcs4_t *)&tx = x;
  *(vcs4_t *)&ty = y;
  for (i = 0 ; i < 4 ; i++) {
    tx[i] = func(tx[i], ty[i]);
  }
  return *(vcs4_t *)&tx;
}

vcd2_t
__ZGVyN2v__mth_i_vc8(vcd2_t x, double complex func(double complex))
{
  int i;
  double complex tx[2];
  *(vcd2_t *)&tx = x;
  for (i = 0 ; i < 2 ; i++) {
    tx[i] = func(tx[i]);
  }
  return *(vcd2_t *)&tx;
}

vcd2_t
__ZGVyN2vv__mth_i_vc8vc8(vcd2_t x, vcd2_t y, double complex func(double complex, double complex))
{
  int i;
  double complex tx[2];
  double complex ty[2];
  *(vcd2_t *)&tx = x;
  *(vcd2_t *)&ty = y;
  for (i = 0 ; i < 2 ; i++) {
    tx[i] = func(tx[i], ty[i]);
  }
  return *(vcd2_t *)&tx;
}
