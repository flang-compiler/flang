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

#ifdef TARGET_OSX_X8664

#include "mth_intrinsics.h"

extern vrs4_t __fs_cos_4_avx2(vrs4_t);
extern vrs4_t __fs_sin_4_avx2(vrs4_t);

vrs4_t
__fs_sincos_4_avx2(vrs4_t x)
{
  vrs4_t ts;
  vrs4_t tc;

  tc = __fs_cos_4_avx2(x);
  ts = __fs_sin_4_avx2(x);
  asm("vmovups\t%0,%%xmm1" : : "m"(tc) : "%xmm1");

  return ts;
}

#else
asm(".globl __fs_sincos_4_avx2\n\
__fs_sincos_4_avx2:\n\
\tpushq	%rbp\n\
\tmovq	%rsp, %rbp\n\
\tsubq	$256, %rsp\n\
\tvmovups	%xmm0, 64(%rsp)\n\
\tcall	__fs_cos_4_avx2@PLT\n\
\tvmovups	%xmm0, 128(%rsp)\n\
\tvmovups	64(%rsp), %xmm0\n\
\tcall	__fs_sin_4_avx2@PLT\n\
\tvmovups	128(%rsp), %xmm1\n\
\tmovq	%rbp, %rsp\n\
\tpopq	%rbp\n\
\tret" );

#endif

