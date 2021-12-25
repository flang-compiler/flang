/*
 * Part of the LLVM Project, under the Apache License v2.0 with LLVM Exceptions.
 * See https://llvm.org/LICENSE.txt for license information.
 * SPDX-License-Identifier: Apache-2.0 WITH LLVM-exception
 *
 */

/* Intrinsic function which take quad precision arguments. */

#if !defined(WIN64)
#include "mthdecls.h"
#else
long double atanl(long double q);
#endif

long double
__mth_i_qatan(long double d)
{
  return atanl(d);
}
