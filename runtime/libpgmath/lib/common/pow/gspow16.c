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

vrs16_t
__gs_pow_16(vrs16_t x, vrs16_t y)
{
  return (__ZGVzN16vv__mth_i_vr4vr4(x, y, __mth_i_rpowr));
}

vrs16_t
__gs_pow_16m(vrs16_t x, vrs16_t y, vis16_t mask)
{
  return (__ZGVzM16vv__mth_i_vr4vr4(x, y, mask, __mth_i_rpowr));
}

vcs8_t
__gc_pow_8(vcs8_t x, vcs8_t y)
{
  return (__ZGVzN8vv__mth_i_vc4vc4(x, y, cpowf));
}
