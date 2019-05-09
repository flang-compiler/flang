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

#include "mthdecls.h"

/* should handle 0**0 w/ exception */

/*
 * __pmth_i_dpowk(float x, long long iy)
 *
 * Return R8**I8 with intermediate terms computed as R8.
 * Most likely used with -Kieee (precise).
 */

/*
 * __float128 is not supported on all platforms so use GCC's long double
 * as a temporary workaround to get higher precision multiplies.
 */

#if defined(TARGET_WIN)
#define __float128 double
#elif	defined (TARGET_LINUX_GENERIC) || defined (TARGET_LINUX_POWER) || defined (LINUX8664) || defined (TARGET_X8664) || defined(TARGET_LINUX_ARM64)
#define	__float128	long double
#endif

double
__pmth_i_dpowk(double x8, long long i8)
{
  long long k;
  __float128 r16;
  __float128 x16;

  r16 = 1.0;
  x16 = x8;
  k = i8;
  if (k < 0)
    k = -k;
  for (;;) {
    if (k & 1)
      r16 *= x16;
    k >>= 1;
    if (k == 0)
      break;
    x16 *= x16;
  }
  if (i8 < 0)
    r16 = 1.0 / r16;
  return r16;
}

/*
 * __pmth_i_dpowi(float x, int i4)
 * Return R8**I4 with intermediate terms computed as r16.
 *
 * Most likely used with -Kieee (precise).
 */

double
__pmth_i_dpowi(double x4, int i4)
{
  return __pmth_i_dpowk(x4, i4);
}
