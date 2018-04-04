
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

vrd4_t
__gd_powi1_4(vrd4_t x, int32_t iy)
{
  return(__ZGVyN4v__mth_i_vr8si4(x, iy, __mth_i_dpowi));
}

vrd4_t
__gd_powi1_4m(vrd4_t x, int32_t iy, vid4_t mask)
{
  return(__ZGVyM4v__mth_i_vr8si4(x, iy, mask, __mth_i_dpowi));
}

vrd4_t
__gd_powi_4(vrd4_t x, vis4_t iy)
{
  return(__ZGVyN4vv__mth_i_vr8vi4(x, iy, __mth_i_dpowi));
}

vrd4_t
__gd_powi_4m(vrd4_t x, vis4_t iy, vid4_t mask)
{
  return(__ZGVyM4vv__mth_i_vr8vi4(x, iy, mask, __mth_i_dpowi));
}

vrd4_t
__gd_powk1_4(vrd4_t x, long long iy)
{
  return(__ZGVyN4v__mth_i_vr8si8(x, iy, __mth_i_dpowk));
}

vrd4_t
__gd_powk1_4m(vrd4_t x, long long iy, vid4_t mask)
{
  return(__ZGVyM4v__mth_i_vr8si8(x, iy, mask, __mth_i_dpowk));
}

vrd4_t
__gd_powk_4(vrd4_t x, vid4_t iy)
{
  return(__ZGVyN4vv__mth_i_vr8vi8(x, iy, __mth_i_dpowk));
}

vrd4_t
__gd_powk_4m(vrd4_t x, vid4_t iy, vid4_t mask)
{
  return(__ZGVyM4vv__mth_i_vr8vi8(x, iy, mask, __mth_i_dpowk));
}
