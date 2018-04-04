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
__gs_atan2_8(vrs8_t x, vrs8_t y)
{
  return (__ZGVyN8vv__mth_i_vr4vr4(x, y, __mth_i_atan2));
}

vrs8_t
__gs_atan2_8m(vrs8_t x, vrs8_t y, vis8_t mask)
{
  return (__ZGVyM8vv__mth_i_vr4vr4(x, y, mask, __mth_i_atan2));
}
