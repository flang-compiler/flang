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

#if defined(TARGET_LINUX_ARM64)

#include "mth_intrinsics.h"

vrd2_t
__gd_sinh_2(vrd2_t x)
{
  return (__ZGVxN2v__mth_i_vr8(x, __mth_i_dsinh));
}

vrd2_t
__gd_sinh_2m(vrd2_t x, vid2_t mask)
{
  return (__ZGVxM2v__mth_i_vr8(x, mask, __mth_i_dsinh));
}

double complex
__gz_sinh_1(double complex x)
{
  return (csinh(x));
}

vcd1_t
__gz_sinh_1v(vcd1_t x)
{
  return (__ZGVxN1v__mth_i_vc8(x, csinh));
}

#endif

