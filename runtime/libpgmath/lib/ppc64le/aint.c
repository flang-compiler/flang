/*
 * Copyright (c) 2014-2017, NVIDIA CORPORATION.  All rights reserved.
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

typedef union {
  float f;
  unsigned int i;
} FPI;

#define EXPBIAS 127
#define MANTBITS 23
#define GET_EXP(u) (int)(((u)&0x7fffffff) >> MANTBITS)

float
__mth_i_aint(float xx)
{
  unsigned int ux, mask;
  int xexp;
  float x;
  FPI fpi;

  x = xx;
  fpi.f = x;
  ux = fpi.i;

  xexp = GET_EXP(ux) - EXPBIAS;
  if (xexp < 0) {
    /* |x| < 0 => zero with the original sign */
    fpi.i = ux & 0x80000000;
  } else if (xexp < MANTBITS) {
    /* 1 <= |x| < 2^24:
     *    just mask out the trailing bits of the mantissa beyond the
     *    range of the exponent; mask out the exponent field as well.
     */
    mask = (1 << (MANTBITS - xexp)) - 1;
    fpi.i = ux & ~mask;
  }
  /* else for illegal input, nan, inf, overflow, ...; just return it */

  return fpi.f;
}
