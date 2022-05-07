/*
 * Part of the LLVM Project, under the Apache License v2.0 with LLVM Exceptions.
 * See https://llvm.org/LICENSE.txt for license information.
 * SPDX-License-Identifier: Apache-2.0 WITH LLVM-exception
 *
 */

/* should handle 0**0 w/ exception */

/*
 * __pmth_i_qpowk(long double x, long long iy)
 *
 * Return R16**I8 with intermediate terms computed as R16.
 * Most likely used with -Kieee (precise).
 */

/*
 * __float128 is not supported on all platforms so use GCC's long double
 * as a temporary workaround to get higher precision multiplies.
 */

#if defined(TARGET_LINUX_GENERIC) || defined(TARGET_LINUX_POWER) ||            \
    defined(LINUX8664) || defined(TARGET_X8664) || defined(TARGET_LINUX_ARM64)
#define __float128 long double
#endif

long double
__pmth_i_qpowk(long double x16, long long i8)
{
  long long k;
  __float128 r16;

  r16 = 1.0;
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
 * __pmth_i_qpowi(long double x, int i4)
 * Return R16**I4 with intermediate terms computed as r16.
 *
 * Most likely used with -Kieee (precise).
 */

long double
__pmth_i_qpowi(long double x16, int i4)
{
  return __pmth_i_qpowk(x16, i4);
}
