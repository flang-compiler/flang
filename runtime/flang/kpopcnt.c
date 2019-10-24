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
__mth_i_kpopcnt(uint64_t u64)
{
  uint64_t r64;

#if     defined(TARGET_X8664)
  asm("popcnt %1, %0"
     : "=r"(r64)
     : "r"(u64)
     );
#elif   defined(TARGET_LINUX_POWER)
    asm("popcntd    %0, %1"
       : "=r"(r64)
       : "r"(u64)
       );
#else
  static const uint64_t u5s = 0x5555555555555555ul;
  static const uint64_t u3s = 0x3333333333333333ul;
  static const uint64_t u7s = 0x0707070707070707ul;
  static const uint64_t u1s = 0x0101010101010101ul;
  r64 = u64;
  r64 = (r64 & u5s) + (r64 >> 1 & u5s);
  r64 = (r64 & u3s) + (r64 >> 2 & u3s);
  r64 = (r64 & u7s) + (r64 >> 4 & u7s);
  r64 *= u1s;
  r64 >>= 56;
#endif

  return r64;
}
