/*
 * Copyright (c) 2017-2018, Arm Ltd.  All rights reserved.
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
__mth_i_ktrailz(int64_t i)
{
  uint64_t ui=i; /* unsigned representation of 'i' */

 #if (defined(PGOCL) || defined(TARGET_LLVM_ARM)) && !defined(TARGET_LLVM_ARM64)
  return (ui)? __builtin_ctz(ui):64;
 #else
   if (!ui) return 64;
   return ((int)(ui))?( __builtin_ctz(ui)): (__builtin_ctz(ui>>32+32));
 #endif
}
