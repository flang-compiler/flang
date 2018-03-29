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

vrs4_t
__ZGVxN4v__mth_i_vr4(vrs4_t x, float func(float))
{
  int i;
  vrs4_t r;
  for (i = 0; i < 4; i++) {
    r[i] = func(x[i]);
  }
  return r;
}

vrs4_t
__ZGVxM4v__mth_i_vr4(vrs4_t x, vis4_t mask, float func(float))
{
  int i;
  vrs4_t r;
  for (i = 0; i < 4; i++) {
    if (mask[i])
      r[i] = func(x[i]);
  }
  return r;
}

vrs4_t
__ZGVxN4vv__mth_i_vr4vr4(vrs4_t x, vrs4_t y, float func(float, float))
{
  int i;
  vrs4_t r;
  for (i = 0; i < 4; i++) {
    r[i] = func(x[i], y[i]);
  }
  return r;
}

vrs4_t
__ZGVxM4vv__mth_i_vr4vr4(vrs4_t x, vrs4_t y, vis4_t mask, float func(float, float))
{
  int i;
  vrs4_t r;
  for (i = 0; i < 4; i++) {
    if (mask[i])
      r[i] = func(x[i], y[i]);
  }
  return r;
}

vrd2_t
__ZGVxN2v__mth_i_vr8(vrd2_t x, double func(double))
{
  int i;
  vrd2_t r;
  for (i = 0; i < 2; i++) {
    r[i] = func(x[i]);
  }
  return r;
}

vrd2_t
__ZGVxM2v__mth_i_vr8(vrd2_t x, vid2_t mask, double func(double))
{
  int i;
  vrd2_t r;
  for (i = 0; i < 2; i++) {
    if (mask[i])
      r[i] = func(x[i]);
  }
  return r;
}

vrd2_t
__ZGVxN2vv__mth_i_vr8vr8(vrd2_t x, vrd2_t y, double func(double, double))
{
  int i;
  vrd2_t r;
  for (i = 0; i < 2; i++) {
    r[i] = func(x[i], y[i]);
  }
  return r;
}

vrd2_t
__ZGVxM2vv__mth_i_vr8vr8(vrd2_t x, vrd2_t y, vid2_t mask, double func(double, double))
{
  int i;
  vrd2_t r;
  for (i = 0; i < 2; i++) {
    if (mask[i])
      r[i] = func(x[i], y[i]);
  }
  return r;
}

vrs4_t
__ZGVxN4v__mth_i_vr4si4(vrs4_t x, int32_t iy, float func(float, int32_t))
{
  int i;
  vrs4_t r;
  for (i = 0 ; i < 4 ; i++) {
    r[i] = func(x[i], iy);
  }
  return r;
}

vrs4_t
__ZGVxM4v__mth_i_vr4si4(vrs4_t x, int32_t iy, vis4_t mask, float func(float, int32_t))
{
  int i;
  vrs4_t r;
  for (i = 0 ; i < 4 ; i++) {
    if (mask[i])
      r[i] = func(x[i], iy);
  }
  return r;
}

vrs4_t
__ZGVxN4vv__mth_i_vr4vi4(vrs4_t x, vis4_t iy, float func(float, int32_t))
{
  int i;
  vrs4_t r;
  for (i = 0 ; i < 4 ; i++) {
    r[i] = func(x[i], iy[i]);
  }
  return r;
}

vrs4_t
__ZGVxM4vv__mth_i_vr4vi4(vrs4_t x, vis4_t iy, vis4_t mask, float func(float, int32_t))
{
  int i;
  vrs4_t r;
  for (i = 0 ; i < 4 ; i++) {
    if (mask[i])
      r[i] = func(x[i], iy[i]);
  }
  return r;
}

vrs4_t
__ZGVxN4v__mth_i_vr4si8(vrs4_t x, long long iy, float func(float, long long))
{
  int i;
  vrs4_t r;
  for (i = 0 ; i < 4 ; i++) {
    r[i] = func(x[i], iy);
  }
  return r;
}

vrs4_t
__ZGVxM4v__mth_i_vr4si8(vrs4_t x, long long iy, vis4_t mask, float func(float, long long))
{
  int i;
  vrs4_t r;
  for (i = 0 ; i < 4 ; i++) {
    if (mask[i])
      r[i] = func(x[i], iy);
  }
  return r;
}

vrs4_t
__ZGVxN4vv__mth_i_vr4vi8(vrs4_t x, vid2_t iyu, vid2_t iyl, float func(float, long long))
{
  int i;
  vrs4_t r;
  for (i = 0 ; i < 2 ; i++) {
    r[i] = func(x[i], iyu[i]);
  }
  for (i = 2 ; i < 4 ; i++) {
    r[i] = func(x[i], iyl[i-2]);
  }
  return r;
}

vrs4_t
__ZGVxM4vv__mth_i_vr4vi8(vrs4_t x, vid2_t iyu, vid2_t iyl, vis4_t mask, float func(float, long long))
{
  int i;
  vrs4_t r;
  for (i = 0 ; i < 2 ; i++) {
    if (mask[i])
      r[i] = func(x[i], iyu[i]);
  }
  for (i = 2 ; i < 4 ; i++) {
    if (mask[i])
      r[i] = func(x[i], iyl[i-2]);
  }
  return r;
}


//---------------


vrd2_t
__ZGVxN2v__mth_i_vr8si4(vrd2_t x, int32_t iy, double func(double, int32_t))
{
  int i;
  vrd2_t r;
  for (i = 0 ; i < 2 ; i++) {
    r[i] = func(x[i], iy);
  }
  return r;
}

vrd2_t
__ZGVxM2v__mth_i_vr8si4(vrd2_t x, int32_t iy, vid2_t mask, double func(double, int32_t))
{
  int i;
  vrd2_t r;
  for (i = 0 ; i < 2 ; i++) {
    if (mask[i])
      r[i] = func(x[i], iy);
  }
  return r;
}

/*
 * __ZGVxN2vv__mth_i_vr8vi4 and __ZGVxM2vv__mth_i_vr8vi4 should
 * be defined as:
 * __ZGVxN2vv__mth_i_vr8vi4(vrd2_t x, vis2_t iy, double func(double, int32_t))
 * __ZGVxM2vv__mth_i_vr8vi4(vrd2_t x, vis2_t iy, vid2_t mask, double func(double, int32_t))
 *
 * But the POWER architectures needs the 32-bit integer vectors to
 * be the full 128-bits of a vector register.
 */

vrd2_t
__ZGVxN2vv__mth_i_vr8vi4(vrd2_t x, vis4_t iy, double func(double, int32_t))
{
  int i;
  vrd2_t r;
  for (i = 0 ; i < 2 ; i++) {
    r[i] = func(x[i], iy[i]);
  }
  return r;
}

vrd2_t
__ZGVxM2vv__mth_i_vr8vi4(vrd2_t x, vis4_t iy, vid2_t mask, double func(double, int32_t))
{
  int i;
  vrd2_t r;
  for (i = 0 ; i < 2 ; i++) {
    if (mask[i])
      r[i] = func(x[i], iy[i]);
  }
  return r;
}

vrd2_t
__ZGVxN2v__mth_i_vr8si8(vrd2_t x, long long iy, double func(double, long long))
{
  int i;
  vrd2_t r;
  for (i = 0 ; i < 2 ; i++) {
    r[i] = func(x[i], iy);
  }
  return r;
}

vrd2_t
__ZGVxM2v__mth_i_vr8si8(vrd2_t x, long long iy, vid2_t mask, double func(double, long long))
{
  int i;
  vrd2_t r;
  for (i = 0 ; i < 2 ; i++) {
    if (mask[i])
      r[i] = func(x[i], iy);
  }
  return r;
}

vrd2_t
__ZGVxN2vv__mth_i_vr8vi8(vrd2_t x, vid2_t iy, double func(double, long long))
{
  int i;
  vrd2_t r;
  for (i = 0 ; i < 2 ; i++) {
    r[i] = func(x[i], iy[i]);
  }
  return r;
}

vrd2_t
__ZGVxM2vv__mth_i_vr8vi8(vrd2_t x, vid2_t iy, vid2_t mask, double func(double, long long))
{
  int i;
  vrd2_t r;
  for (i = 0 ; i < 2 ; i++) {
    if (mask[i])
      r[i] = func(x[i], iy[i]);
  }
  return r;
}


vcs1_t
__ZGVxN1v__mth_i_vc4(vcs1_t x, float complex func(float complex))
{
  int i;
  float complex tx;
  *(vcs1_t *)&tx = x;
  tx = func(tx);
  return *(vcs1_t *)&tx;
}

vcs1_t
__ZGVxN1vv__mth_i_vc4vc4(vcs1_t x, vcs1_t y, float complex func(float complex, float complex))
{
  int i;
  float complex tx;
  float complex ty;
  *(vcs1_t *)&tx = x;
  *(vcs1_t *)&ty = y;
  tx = func(tx, ty);
  return *(vcs1_t *)&tx;
}

vcs2_t
__ZGVxN2v__mth_i_vc4(vcs2_t x, float complex func(float complex))
{
  int i;
  float complex tx[2];
  *(vcs2_t *)&tx = x;
  for (i = 0 ; i < 2 ; i++) {
    tx[i] = func(tx[i]);
  }
  return *(vcs2_t *)&tx;
}

vcs2_t
__ZGVxN2vv__mth_i_vc4vc4(vcs2_t x, vcs2_t y, float complex func(float complex, float complex))
{
  int i;
  float complex tx[2];
  float complex ty[2];
  *(vcs2_t *)&tx = x;
  *(vcs2_t *)&ty = y;
  for (i = 0 ; i < 2 ; i++) {
    tx[i] = func(tx[i], ty[i]);
  }
  return *(vcs2_t *)&tx;
}

vcd1_t
__ZGVxN1v__mth_i_vc8(vcd1_t x, double complex func(double complex))
{
  int i;
  double complex tx;
  *(vcd1_t *)&tx = x;
  tx = func(tx);
  return *(vcd1_t *)&tx;
}

vcd1_t
__ZGVxN1vv__mth_i_vc8vc8(vcd1_t x, vcd1_t y, double complex func(double complex, double complex))
{
  int i;
  double complex tx;
  double complex ty;
  *(vcd1_t *)&tx = x;
  *(vcd1_t *)&ty = y;
  tx = func(tx, ty);
  return *(vcd1_t *)&tx;
}

vcs1_t
__ZGVxN1v__mth_i_vc4si4(vcs1_t x, int32_t iy, float complex func(float complex, int32_t))
{
  int i;
  float complex tx;
  *(vcs1_t *)&tx = x;
  tx = func(tx, iy);
  return *(vcs1_t *)&tx;
}

vcs1_t
__ZGVxN1v__mth_i_vc4si8(vcs1_t x, long long iy, float complex func(float complex, long long))
{
  int i;
  float complex tx;
  *(vcs1_t *)&tx = x;
  tx = func(tx, iy);
  return *(vcs1_t *)&tx;
}

vcd1_t
__ZGVxN1v__mth_i_vc8si4(vcd1_t x, int32_t iy, double complex func(double complex, int32_t))
{
  int i;
  double complex tx;
  *(vcd1_t *)&tx = x;
  tx = func(tx, iy);
  return *(vcd1_t *)&tx;
}

vcd1_t
__ZGVxN1v__mth_i_vc8si8(vcd1_t x, long long iy, double complex func(double complex, long long))
{
  int i;
  double complex tx;
  *(vcd1_t *)&tx = x;
  tx = func(tx, iy);
  return *(vcd1_t *)&tx;
}
