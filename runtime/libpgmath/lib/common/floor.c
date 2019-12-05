/*
 * Part of the LLVM Project, under the Apache License v2.0 with LLVM Exceptions.
 * See https://llvm.org/LICENSE.txt for license information.
 * SPDX-License-Identifier: Apache-2.0 WITH LLVM-exception
 *
 */

#include "mthdecls.h"
#if     defined(__SSE4_1__) || defined(__AVX__)
#include    <immintrin.h>
#endif

#if     defined(__AVX__)
float
__mth_i_floor_avx(float x)
{
  return _mm_cvtss_f32(_mm_floor_ss(_mm_set1_ps(x), _mm_set1_ps(x)));
}
#elif   defined(__SSE4_1__)
float
__mth_i_floor_sse(float x)
{
  return _mm_cvtss_f32(_mm_floor_ss(_mm_set1_ps(x), _mm_set1_ps(x)));
}
#else
float
__mth_i_floor(float x)
{
  return floorf(x);
}
#endif
