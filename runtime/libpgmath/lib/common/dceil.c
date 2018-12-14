/*
 * Copyright (c) 2018, NVIDIA CORPORATION.  All rights reserved.
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
