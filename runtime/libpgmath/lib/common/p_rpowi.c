/*
 * Copyright (c) 2017, NVIDIA CORPORATION.  All rights reserved.
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

/* should handle 0**0 w/ exception */

/*
 * __pmth_i_rpowk(float x, long long iy)
 * Return R4**I8 with intermediate terms computed as R8.
 *
 * Most likely used with -Kieee (precise).
 */

float
__pmth_i_rpowk(float x4, long long i8)
{
  long long k;
  double r8;
  double x8;

  r8 = 1.0;
  x8 = x4;
  k = i8;
  if (k < 0)
    k = -k;
  for (;;) {
    if (k & 1)
      r8 *= x8;
    k >>= 1;
    if (k == 0)
      break;
    x8 *= x8;
  }
  if (i8 < 0)
    r8 = 1.0 / r8;
  return r8;
}

/*
 * __pmth_i_rpowi(float x, int i4)
 * Return R4**I4 with intermediate terms computed as R8.
 *
 * Most likely used with -Kieee (precise).
 */

float
__pmth_i_rpowi(float x4, int i4)
{
  return __pmth_i_rpowk(x4, i4);
}
