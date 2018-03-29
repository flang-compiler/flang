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

extern vrd8_t __fd_cos_8_avx512(vrd8_t);
extern vrd8_t __fd_sin_8_avx512(vrd8_t);

vrd8_t
__fd_sincos_8_avx512(vrd8_t x)
{
  vrd8_t ts;
  vrd8_t tc;

  tc = __fd_cos_8_avx512(x);
  ts = __fd_sin_8_avx512(x);
  asm("vmovupd\t%0,%%zmm1" : : "m"(tc) : "%zmm1");

  return ts;
}

#else
asm(".globl __fd_sincos_8_avx512\n\
__fd_sincos_8_avx512:\n\
\tpushq	%rbp\n\
\tmovq	%rsp, %rbp\n\
\tsubq	$256, %rsp\n\
\tvmovupd	%zmm0, 64(%rsp)\n\
\tcall	__fd_cos_8_avx512@PLT\n\
\tvmovupd	%zmm0, 128(%rsp)\n\
\tvmovupd	64(%rsp), %zmm0\n\
\tcall	__fd_sin_8_avx512@PLT\n\
\tvmovupd	128(%rsp), %zmm1\n\
\tmovq	%rbp, %rsp\n\
\tpopq	%rbp\n\
\tret" );

#endif

