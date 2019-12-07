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

float
__mth_i_ceil(float x)
{
  return ceilf(x);
}
