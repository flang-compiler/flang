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
double
__mth_i_dceil_avx(double x)
{
  return _mm_cvtsd_f64(_mm_ceil_sd(_mm_set1_pd(x), _mm_set1_pd(x)));
}
#elif   defined(__SSE4_1__)
double
__mth_i_dceil_sse(double x)
{
  return _mm_cvtsd_f64(_mm_ceil_sd(_mm_set1_pd(x), _mm_set1_pd(x)));
}
#else
double
__mth_i_dceil(double x)
{
  return ceil(x);
}
#endif
