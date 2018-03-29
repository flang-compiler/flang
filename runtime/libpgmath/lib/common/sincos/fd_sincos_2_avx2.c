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

extern vrd2_t __fd_cos_2_avx2(vrd2_t);
extern vrd2_t __fd_sin_2_avx2(vrd2_t);

vrd2_t
__fd_sincos_2_avx2(vrd2_t x)
{
  vrd2_t ts;
  vrd2_t tc;

  tc = __fd_cos_2_avx2(x);
  ts = __fd_sin_2_avx2(x);
  asm("vmovupd\t%0,%%xmm1" : : "m"(tc) : "%xmm1");

  return ts;
}

#else
asm(".globl __fd_sincos_2_avx2\n\
__fd_sincos_2_avx2:\n\
\tpushq	%rbp\n\
\tmovq	%rsp, %rbp\n\
\tsubq	$256, %rsp\n\
\tvmovupd	%xmm0, 64(%rsp)\n\
\tcall	__fd_cos_2_avx2@PLT\n\
\tvmovupd	%xmm0, 128(%rsp)\n\
\tvmovupd	64(%rsp), %xmm0\n\
\tcall	__fd_sin_2_avx2@PLT\n\
\tvmovupd	128(%rsp), %xmm1\n\
\tmovq	%rbp, %rsp\n\
\tpopq	%rbp\n\
\tret" );

#endif

