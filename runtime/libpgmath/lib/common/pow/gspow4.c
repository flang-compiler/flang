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
__gs_pow_4(vrs4_t x, vrs4_t y)
{
  return (__ZGVxN4vv__mth_i_vr4vr4(x, y, __mth_i_rpowr));
}

vrs4_t
__gs_pow_4m(vrs4_t x, vrs4_t y, vis4_t mask)
{
  return (__ZGVxM4vv__mth_i_vr4vr4(x, y, mask, __mth_i_rpowr));
}

vcs1_t
__gc_pow_1(vcs1_t x, vcs1_t y)
{
  return (__ZGVxN1vv__mth_i_vc4vc4(x, y, cpowf));
}

vcs2_t
__gc_pow_2(vcs2_t x, vcs2_t y)
{
  return (__ZGVxN2vv__mth_i_vc4vc4(x, y, cpowf));
}
