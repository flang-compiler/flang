/*
 * Copyright (c) 2017-2019, NVIDIA CORPORATION.  All rights reserved.
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

#include <stdint.h>

int32_t
__mth_i_ipopcnt(uint32_t u32)
{
  uint32_t r32;

#if     defined(TARGET_X8664)
    asm("popcnt %1, %0"
       : "=r"(r32)
       : "r"(u32)
       :
       );
#elif   defined(TARGET_LINUX_POWER)
    asm("popcntw    %0, %1"
       : "=r"(r32)
       : "r"(u32)
       :
       );
#else
  static const uint32_t u5s = 0x55555555;
  static const uint32_t u3s = 0x33333333;
  static const uint32_t u7s = 0x07070707;
  static const uint32_t u1s = 0x01010101;

  r32 = u32;
  r32 = (r32 & u5s) + (r32 >> 1 & u5s);
  r32 = (r32 & u3s) + (r32 >> 2 & u3s);
  r32 = (r32 & u7s) + (r32 >> 4 & u7s);
  r32 *= u1s;
  r32 >>= 24;
#endif

  return r32;
}
