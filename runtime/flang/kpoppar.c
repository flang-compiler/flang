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

int64_t
__mth_i_kpoppar(uint64_t u64)
{
  uint64_t r64;

#if     defined(TARGET_X8664)
    asm("popcnt %1, %0\n"
        "\tandq $0x1, %0"
       : "=r"(r64)
       : "r"(u64)
       :
       );
#elif   defined(TARGET_LINUX_POWER)
    asm("popcntd    %0, %1\n"
        "\trldicl   %0, %0, 0, 63"
       : "=r"(r64)
       : "r"(u64)
       :
       );
#else
  r64 = u64;
  r64 ^= r64 >> 32;
  r64 ^= r64 >> 16;
  r64 ^= r64 >> 8;
  r64 ^= r64 >> 4;
  r64 ^= r64 >> 2;
  r64 ^= r64 >> 1;
  r64 &= 0x1;
#endif

  return r64;
}
