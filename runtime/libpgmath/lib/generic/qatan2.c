/*
 * Part of the LLVM Project, under the Apache License v2.0 with LLVM Exceptions.
 * See https://llvm.org/LICENSE.txt for license information.
 * SPDX-License-Identifier: Apache-2.0 WITH LLVM-exception
 *
 */

#if !defined(WIN64)
#include "mthdecls.h"
#else
long double atan2(long double x, long double y);
#endif

long double
__mth_i_qatan2(long double x, long double y)
{
  return atan2l(x, y);
}
