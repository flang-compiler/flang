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
__mth_i_ipoppar(uint32_t u32)
{
  uint32_t r32;

#if     defined(TARGET_X8664)
    asm("popcnt %1, %0\n"
        "\tandl $0x1, %0"
       : "=r"(r32)
       : "r"(u32)
       :
       );
#elif   defined(TARGET_LINUX_POWER)
    asm("popcntw    %0, %1\n"
        "\trldicl   %0, %0, 0, 63"
       : "=r"(r32)
       : "r"(u32)
       :
       );
#else
  r32 = u32;
  r32 ^= r32 >> 16;
  r32 ^= r32 >> 8;
  r32 ^= r32 >> 4;
  r32 ^= r32 >> 2;
  r32 ^= r32 >> 1;
  r32 &= 0x1;
#endif

  return r32;
}
