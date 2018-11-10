/*
 * Copyright (c) 2014-2018, NVIDIA CORPORATION.  All rights reserved.
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
#include "mthdecls.h"

typedef union {
  double f;
  uint64_t i;
} FPI;

#define EXPBIAS 1023
#define MANTBITS 52
#define GET_EXP(u) (int64_t)(((u)&0x7ff0000000000000) >> MANTBITS)

double
__mth_i_dint(double xx)
{
  int64_t xexp;
  uint64_t ux, mask;
  double x;
  FPI fpi;

  x = xx;
  fpi.f = x;
  ux = fpi.i;
  xexp = GET_EXP(ux) - EXPBIAS;
  if (xexp < 0) {
    /* |x| < 0  =>  zero with the original sign */
    fpi.i = (ux & 0x8000000000000000);
  } else if (xexp < MANTBITS) {
    /* 1 <= |x| < 2^53:
     *    just mask out the trailing bits of the mantiassa beyond the
     *    range of the exponent; mask out the exponent field as well.
     */
    mask = ((uint64_t)1 << (MANTBITS - xexp)) - 1;
    fpi.i = ux & ~mask;
  }
  /* else illegal input, nan, inf, overflow, ...; just return it */

  return fpi.f;
}
