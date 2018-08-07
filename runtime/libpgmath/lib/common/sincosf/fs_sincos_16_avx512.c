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

#define CONFIG 1
#include "helperavx512f.h"


#ifdef TARGET_OSX_X8664

#include "mth_intrinsics.h"

extern vrs16_t FCN_AVX512(__fs_cos_16)(vrs16_t);
extern vrs16_t FCN_AVX512(__fs_sin_16)(vrs16_t);

vrs16_t
FCN_AVX512(__fs_sincos_16)(vrs16_t x)
{
  vrs16_t ts;
  vrs16_t tc;

  tc = FCN_AVX512(__fs_cos_16)(x);
  ts = FCN_AVX512(__fs_sin_16)(x);
  asm("vmovups\t%0,%%zmm1" : : "m"(tc) : "%zmm1");

  return ts;
}

#else

asm(".globl "STRINGIFY(FCN_AVX512(__fs_sincos_16))"\n\
"STRINGIFY(FCN_AVX512(__fs_sincos_16))":\n\
\tpushq	%rbp\n\
\tmovq	%rsp, %rbp\n\
\tsubq	$256, %rsp\n\
\tvmovups	%zmm0, 64(%rsp)\n\
\tcall	"STRINGIFY(FCN_AVX512(__fs_cos_16))"@PLT\n\
\tvmovups	%zmm0, 128(%rsp)\n\
\tvmovups	64(%rsp), %zmm0\n\
\tcall	"STRINGIFY(FCN_AVX512(__fs_sin_16))"@PLT\n\
\tvmovups	128(%rsp), %zmm1\n\
\tmovq	%rbp, %rsp\n\
\tpopq	%rbp\n\
\tret" );

#endif

