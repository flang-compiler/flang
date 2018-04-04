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

vrd4_t
__gd_pow_4(vrd4_t x, vrd4_t y)
{
  return (__ZGVyN4vv__mth_i_vr8vr8(x, y, __mth_i_dpowd));
}

vrd4_t
__gd_pow_4m(vrd4_t x, vrd4_t y, vid4_t mask)
{
  return (__ZGVyM4vv__mth_i_vr8vr8(x, y, mask, __mth_i_dpowd));
}

vcd2_t
__gz_pow_2(vcd2_t x, vcd2_t y)
{
  return (__ZGVyN2vv__mth_i_vc8vc8(x, y, cpow));
}
