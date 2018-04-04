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

vrd8_t
__gd_atan2_8(vrd8_t x, vrd8_t y)
{
  return (__ZGVzN8vv__mth_i_vr8vr8(x, y, __mth_i_datan2));
}

vrd8_t
__gd_atan2_8m(vrd8_t x, vrd8_t y, vid8_t mask)
{
  return (__ZGVzM8vv__mth_i_vr8vr8(x, y, mask, __mth_i_datan2));
}
