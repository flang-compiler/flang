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

vcs1_t
__gc_div_1(vcs1_t x, vcs1_t y)
{
  return (__ZGVxN1vv__mth_i_vc4vc4(x, y, __mth_i_cdiv_c99));
}

vcs2_t
__gc_div_2(vcs2_t x, vcs2_t y)
{
  return (__ZGVxN2vv__mth_i_vc4vc4(x, y, __mth_i_cdiv_c99));
}

double complex
__gz_div_1(double complex x, double complex y)
{
  return (__mth_i_cddiv_c99(x, y));
}

vcd1_t
__gz_div_1v(vcd1_t x, vcd1_t y)
{
  return (__ZGVxN1vv__mth_i_vc8vc8(x, y, __mth_i_cddiv_c99));
}
